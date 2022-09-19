{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ZhArchiver.Image
  ( ImgRef,
    Image (..),
    imgFromUrl,
    imgLocalPath,
    poImage,
    poImageMaybe,
    ImgMap,
    emptyImgMap,
    lookupLocalPath,
    FileMap,
    emptyFileMap,
    saveImgFiles,
    ImgFetcher,
    runImgFetcher,
    HasImage (..),
    getImage,
    getHtmlImages,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
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
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple (swap)
import Network.HTTP.Req
import Network.HTTP.Types.Header
import Network.Mime (defaultMimeMap)
import System.Directory
import System.FilePath
import System.Posix.Files
import Text.HTML.TagSoup
import Text.URI
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.RawParser.TH
import ZhArchiver.RawParser.Util

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
    refImgFile :: FilePath,
    refImgDigest :: ImgDigest
  }
  deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''ImgRef

instance Hashable ImgRef where
  hashWithSalt s i = hashWithSalt s (refImgType i, refImgDigest i)

toLocalPath :: FilePath -> ImgRef -> Text
toLocalPath store ImgRef {refImgFile = f} = T.pack (store </> f)

data Image = Image
  { imgUrl :: Text,
    imgRef :: Maybe ImgRef
  }
  deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Image

imgFromUrl :: Text -> Image
imgFromUrl u = Image {imgUrl = u, imgRef = Nothing}

imgLocalPath :: FilePath -> Image -> Text
imgLocalPath p im = maybe (imgUrl im) (toLocalPath p) (imgRef im)

poImage :: ParseOpt
poImage = PoMap [|imgFromUrl|]

poImageMaybe :: ParseOpt
poImageMaybe = PoMap [|appUnless T.null imgFromUrl|]

-- | map from url to (type, hash) (sha256)
newtype ImgMap = ImgHash (HashMap Text ImgRef)
  deriving (Eq, Show)

deriveJSON defaultOptions ''ImgMap

emptyImgMap :: ImgMap
emptyImgMap = ImgHash HM.empty

lookupLocalPath :: FilePath -> ImgMap -> Text -> Text
lookupLocalPath store (ImgHash mp) url = maybe url (toLocalPath store) (HM.lookup url mp)

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
          ( \(ImgRef {refImgFile = path}, dat) ->
              doesFileExist path
                >>= \e -> unless e $ do
                  BS.writeFile path dat
                  setFileMode path readonly
          )
          (HM.toList m)
      )
  where
    readonly = foldl1 unionFileModes [ownerReadMode, groupReadMode, otherReadMode]

type ImgFetcher m = StateT FileMap m

runImgFetcher :: ImgFetcher m a -> m (a, FileMap)
runImgFetcher f = runStateT f emptyFileMap

class HasImage a where
  fetchImage :: (MonadHttp m, MonadCatch m) => Cli -> a -> ImgFetcher m a

instance HasImage Image where
  fetchImage cli im =
    (\r -> im {imgRef = r}) <$> getImage (imgUrl im)
      <* liftIO (showProgress cli "Download image file" <* endProgress cli)

instance (HasImage a) => HasImage (Maybe a) where
  fetchImage cli = traverse (fetchImage cli)

instance (HasImage a, ShowId a) => HasImage [a] where
  fetchImage cli = traverseP cli fetchImage

instance (HasImage a) => HasImage (WithRaw a) where
  fetchImage cli v = (\nv -> v {wrVal = nv}) <$> fetchImage cli (wrVal v)

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
                hsh = hash body
                ref =
                  ImgRef
                    { refImgType = TE.decodeUtf8 <$> hdr,
                      refImgFile =
                        let baseName = show hsh
                         in maybe
                              baseName
                              (addExtension baseName . T.unpack)
                              (hdr >>= \v -> HM.lookup v mimeToExt),
                      refImgDigest = ImgDigest hsh
                    }
             in do
                  modify (\(ImgFiles f) -> ImgFiles (HM.insert ref body f))
                  return (Just ref)
        )
        (const (pure Nothing))
    Nothing -> pure Nothing

getHtmlImages :: (MonadHttp m, MonadCatch m) => Cli -> Text -> ImgFetcher m ImgMap
getHtmlImages cli htm =
  let imgs = getSrc htm
      num = length imgs
   in ImgHash . HM.fromList . catMaybes
        <$> ( traverse
                ( \(url, idx) ->
                    fmap (url,)
                      <$> ( liftIO (showProgress cli ("image " ++ show idx ++ "/" ++ show num))
                              *> getImage url
                          )
                )
                (zip imgs [(1 :: Int) ..])
                <* liftIO (unless (null imgs) $ endProgress cli)
            )
  where
    getSrc =
      mapMaybe
        ( \case
            TagOpen "img" attr -> lookup "src" attr
            _ -> Nothing
        )
        . parseTags