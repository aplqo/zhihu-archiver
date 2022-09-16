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
    saveImgFiles,
    ImgFetcher,
    HasImage (..),
    getImage,
    getHtmlImages,
  )
where

import Control.Monad.Catch
import Control.Monad.State
import Crypto.Hash
import Data.Aeson hiding (String, Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (original)
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple (swap)
import qualified Data.Vector as V
import Network.HTTP.Req
import Network.HTTP.Types.Header
import Network.Mime (defaultMimeMap)
import System.Directory
import System.FilePath
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
    refImgExt :: Maybe Text,
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

deriveJSON defaultOptions ''ImgMap

emptyImgMap :: ImgMap
emptyImgMap = ImgHash HM.empty

-- | map from (type, hash) to content
newtype FileMap = ImgFiles (HashMap ImgRef ByteString)
  deriving (Eq, Show)

emptyFileMap :: FileMap
emptyFileMap = ImgFiles HM.empty

saveImgFiles :: FilePath -> FileMap -> IO ()
saveImgFiles pat (ImgFiles m) =
  createDirectoryIfMissing False pat
    >> withCurrentDirectory
      pat
      ( traverse_
          ( \(ref, dat) ->
              let baseName = show (refImgDigest ref)
                  path = maybe baseName (addExtension baseName . T.unpack) (refImgExt ref)
               in doesFileExist path
                    >>= \e -> unless e (BS.writeFile path dat)
          )
          (HM.toList m)
      )

type ImgFetcher m v = StateT FileMap m v

class HasImage a where
  fetchImage :: (MonadHttp m, MonadCatch m) => a -> ImgFetcher m a

instance HasImage Image where
  fetchImage im = (\r -> im {imgRef = r}) <$> getImage (imgUrl im)

instance (HasImage a) => HasImage (Maybe a) where
  fetchImage = traverse fetchImage

instance (HasImage a) => HasImage [a] where
  fetchImage = traverse fetchImage

mimeToExt :: HashMap ByteString Text
mimeToExt = (HM.fromList . fmap swap . M.toList) defaultMimeMap

getImage :: (MonadHttp m, MonadCatch m) => Text -> ImgFetcher m (Maybe ImgRef)
getImage url =
  case mkURI url >>= useURI of
    Just u ->
      catchAll
        ( do
            dat <- case u of
              Left (h, o) -> req GET h NoReqBody bsResponse o
              Right (hs, o) -> req GET hs NoReqBody bsResponse o
            let body = responseBody dat
                hdr = responseHeader dat (original hContentType)
                ref =
                  ImgRef
                    { refImgType = TE.decodeUtf8 <$> hdr,
                      refImgExt = hdr >>= \v -> HM.lookup v mimeToExt,
                      refImgDigest = ImgDigest (hash body)
                    }
             in do
                  modify (\(ImgFiles f) -> ImgFiles (HM.insert ref body f))
                  return (Just ref)
        )
        (const (pure Nothing))
    Nothing -> pure Nothing

getHtmlImages :: (MonadHttp m, MonadCatch m) => Text -> ImgFetcher m ImgMap
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