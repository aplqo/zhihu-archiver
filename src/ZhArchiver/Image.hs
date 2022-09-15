{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ZhArchiver.Image
  ( ImgMap,
    emptyImgMap,
    FileMap,
    emptyFileMap,
    httpConfig,
    getImages,
  )
where

import Control.Monad.State
import Crypto.Hash
import Data.Aeson hiding (String, Value, defaultOptions)
import qualified Data.Aeson as JSON
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (singleton)
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

-- | map from url to (type, hash) (sha256)
newtype ImgMap = ImgHash (HashMap Text (Maybe Text, ImgDigest))
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
                      t <- o .:? "content_type"
                      h <- o .: "sha256"
                      return (u, (t, h))
                )
            )
      )

instance ToJSON ImgMap where
  toJSON (ImgHash ih) =
    Array
      ( V.fromList
          ( ( \(k, (t, d)) ->
                object
                  ( maybe [] (singleton . ("content_type" .=)) t
                      ++ ["url" .= k, "sha256" .= d]
                  )
            )
              <$> HM.toList ih
          )
      )

-- | map from (type, hash) to content
newtype FileMap = ImgFiles (HashMap (Maybe Text, ImgDigest) ByteString)
  deriving (Eq, Show)

emptyFileMap :: FileMap
emptyFileMap = ImgFiles HM.empty

httpConfig :: HttpConfig
httpConfig = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}

getImages :: MonadHttp m => Text -> StateT FileMap m ImgMap
getImages htm =
  let imgs = getSrc htm
   in ImgHash . HM.fromList . collect
        <$> traverse getImg imgs
  where
    collect :: [Maybe a] -> [a]
    collect =
      foldr (\i c -> maybe c (: c) i) []

    getSrc =
      collect
        . fmap
          ( \case
              TagOpen "img" attr ->
                lookup "src" attr >>= \s -> (s,) <$> (mkURI s >>= useURI)
              _ -> Nothing
          )
        . parseTags

    getImg (src, u) =
      ( do
          dat <- case u of
            Left (h, o) -> req GET h NoReqBody bsResponse o
            Right (hs, o) -> req GET hs NoReqBody bsResponse o
          if responseStatusCode dat == 200
            then
              let body = responseBody dat
                  typ = TE.decodeUtf8 <$> responseHeader dat (original hContentType)
                  hsh = ImgDigest (hash body)
               in do
                    modify (\(ImgFiles f) -> ImgFiles (HM.insert (typ, hsh) body f))
                    return (Just (src, (typ, hsh)))
            else return Nothing
      ) ::
        (MonadHttp m => StateT FileMap m (Maybe (Text, (Maybe Text, ImgDigest))))