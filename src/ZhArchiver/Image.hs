{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ZhArchiver.Image
  ( ImgRef,
    Image (..),
    ImgMap,
    emptyImgMap,
    FileMap,
    emptyFileMap,
    httpConfig,
    ImgFetcher,
    HasImage (..),
    getImage,
    getHtmlImages,
  )
where

import Control.Monad.State
import Crypto.Hash
import Data.Aeson hiding (String, Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Req
import Network.HTTP.Types.Header
import Text.HTML.TagSoup
import Text.URI

newtype ImgDigest = ImgDigest (Digest SHA256)
  deriving newtype (Show)
  deriving (Eq)

instance FromJSON ImgDigest where
  parseJSON = withText "SHA256 hash" (pure . ImgDigest . read . T.unpack)

instance ToJSON ImgDigest where
  toJSON (ImgDigest d) = JSON.String (T.pack (show d))

instance Hashable ImgDigest where
  hashWithSalt s (ImgDigest d) = hashWithSalt s (BA.unpack d)

data ImgRef = ImgRef
  { refImgType :: Maybe Text,
    refImgDigest :: ImgDigest
  }
  deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''ImgRef

instance Hashable ImgRef where
  hashWithSalt s i = hashWithSalt s (refImgType i, refImgDigest i)

data Image = Image
  { imgUrl :: Text,
    imgRef :: Maybe ImgRef
  }
  deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Image

-- | map from url to (type, hash) (sha256)
newtype ImgMap = ImgHash (HashMap Text ImgRef)
  deriving (Eq, Show)

emptyImgMap :: ImgMap
emptyImgMap = ImgHash HM.empty

instance FromJSON ImgMap where
  parseJSON =
    withArray
      "ImgHash"
      ( fmap (ImgHash . HM.fromList . V.toList)
          . V.mapM
            ( withObject
                "Hash entity"
                ( \o ->
                    do
                      u <- o .: "url"
                      r <- o .: "ref"
                      return (u, r)
                )
            )
      )

instance ToJSON ImgMap where
  toJSON (ImgHash ih) =
    Array
      ( V.fromList
          ( (\(k, r) -> object ["url" .= k, "ref" .= r])
              <$> HM.toList ih
          )
      )

-- | map from (type, hash) to content
newtype FileMap = ImgFiles (HashMap ImgRef ByteString)
  deriving (Eq, Show)

emptyFileMap :: FileMap
emptyFileMap = ImgFiles HM.empty

httpConfig :: HttpConfig
httpConfig = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}

type ImgFetcher m v = StateT FileMap m v

class HasImage a where
  fetchImage :: MonadHttp m => a -> ImgFetcher m a

instance HasImage Image where
  fetchImage im = (\r -> im {imgRef = r}) <$> getImage (imgUrl im)

instance (HasImage a) => HasImage (Maybe a) where
  fetchImage = traverse fetchImage

instance (HasImage a) => HasImage [a] where
  fetchImage = traverse fetchImage

getImage :: MonadHttp m => Text -> ImgFetcher m (Maybe ImgRef)
getImage url =
  case mkURI url >>= useURI of
    Just u ->
      do
        dat <- case u of
          Left (h, o) -> req GET h NoReqBody bsResponse o
          Right (hs, o) -> req GET hs NoReqBody bsResponse o
        if responseStatusCode dat == 200
          then
            let body = responseBody dat
                ref =
                  ImgRef
                    { refImgType = TE.decodeUtf8 <$> responseHeader dat (original hContentType),
                      refImgDigest = ImgDigest (hash body)
                    }
             in do
                  modify (\(ImgFiles f) -> ImgFiles (HM.insert ref body f))
                  return (Just ref)
          else return Nothing
    Nothing -> pure Nothing

getHtmlImages :: MonadHttp m => Text -> ImgFetcher m ImgMap
getHtmlImages htm =
  let imgs = getSrc htm
   in ImgHash . HM.fromList . collect
        <$> traverse (\url -> fmap (url,) <$> getImage url) imgs
  where
    collect :: [Maybe a] -> [a]
    collect =
      foldr (\i c -> maybe c (: c) i) []

    getSrc =
      collect
        . fmap
          ( \case
              TagOpen "img" attr -> lookup "src" attr
              _ -> Nothing
          )
        . parseTags