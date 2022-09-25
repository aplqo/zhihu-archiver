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
    elemImages,
    lookupLocalPath,
    LinkMap,
    emptyLM,
    loadLinks,
    storeLinks,
    checkLinks,
    FileMap,
    emptyFileMap,
    ImgSaver,
    runImgSaver,
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
import Data.Bifunctor
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (original)
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
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
import System.Posix.Files hiding (isSymbolicLink)
import Text.HTML.TagSoup
import Text.URI hiding (makeAbsolute)
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Raw.Parser.Util
import ZhArchiver.Util
import ZhArchiver.Util.FilePath

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

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6} ''ImgRef

instance Hashable ImgRef where
  hashWithSalt s i = hashWithSalt s (refImgType i, refImgDigest i)

mimeToExt :: HashMap Text FilePath
mimeToExt = (HM.fromList . fmap (bimap TE.decodeUtf8 T.unpack . swap) . M.toList) defaultMimeMap

refFile :: ImgRef -> FilePath
refFile ImgRef {refImgDigest = ImgDigest hsh, refImgType = typ} =
  let baseName = show hsh
   in maybe
        baseName
        (addExtension baseName)
        (typ >>= \v -> HM.lookup v mimeToExt)

toLocalPath :: FilePath -> ImgRef -> Text
toLocalPath store ref = T.pack (store </> refFile ref)

data Image = Image
  { imgUrl :: Text,
    imgRef :: Maybe ImgRef
  }
  deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''Image

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

elemImages :: ImgMap -> HS.HashSet ImgRef
elemImages (ImgHash ih) = HS.fromList (HM.elems ih)

lookupLocalPath :: FilePath -> ImgMap -> Text -> Text
lookupLocalPath store (ImgHash mp) url = maybe url (toLocalPath store) (HM.lookup url mp)

newtype LinkMap = FL {unFL :: HashMap FilePath (HS.HashSet FilePath)}
  deriving (Eq, Show)

emptyLM :: LinkMap
emptyLM = FL HM.empty

storeLinks :: FilePath -> LinkMap -> IO ()
storeLinks p (FL fl) =
  traverse (traverse (makeRelativeEx p) . HS.toList) fl
    >>= encodeFilePretty True (p </> "image_link.json")

loadLinks :: FilePath -> Decoder LinkMap
loadLinks p =
  decodeFile (p </> "image_link.json")
    >>= fmap FL
      . traverse
        ( fmap HS.fromList
            . traverse (\d -> liftIO (makeAbsolute (p </> d)))
        )

checkLinks :: LinkMap -> IO LinkMap
checkLinks (FL lm) =
  FL . HM.filter (not . HS.null)
    <$> traverse (fmap HS.fromList . filterM checkLink . HS.toList) lm
  where
    checkLink l =
      doesPathExist l >>= \e ->
        if e
          then do
            sym <- pathIsSymbolicLink l
            if sym
              then readSymbolicLink l >>= checkLink
              else return True
          else return False

insertLink :: FilePath -> FilePath -> LinkMap -> LinkMap
insertLink k v (FL l) = FL (HM.insertWith HS.union k (HS.singleton v) l)

-- | map from (type, hash) to content
newtype FileMap = ImgFiles (HashMap ImgRef ByteString)
  deriving (Eq, Show)

emptyFileMap :: FileMap
emptyFileMap = ImgFiles HM.empty

type ImgSaver m = StateT LinkMap m

runImgSaver :: ImgSaver m a -> LinkMap -> m (a, LinkMap)
runImgSaver = runStateT

saveImgFiles :: (MonadIO m) => FilePath -> FileMap -> ImgSaver m ()
saveImgFiles pat (ImgFiles m) =
  liftIO (createDirectoryIfMissing False pat)
    >> traverse_
      ( \(ref, dat) ->
          let name = refFile ref
              path = pat </> name
           in liftIO (doesFileExist path)
                >>= \e -> unless e $ do
                  lks <- gets (fmap (head . HS.toList) . HM.lookup name . unFL)
                  liftIO $ case lks of
                    Just p -> do
                      tar <- readLink p
                      createLink tar path
                    Nothing -> do
                      BS.writeFile path dat
                      setFileMode path readonly
                  absP <- liftIO $ makeAbsolute path
                  modify (insertLink name absP)
      )
      (HM.toList m)
  where
    readonly = foldl1 unionFileModes [ownerReadMode, groupReadMode, otherReadMode]

    readLink :: FilePath -> IO FilePath
    readLink p =
      pathIsSymbolicLink p >>= \sym ->
        if sym
          then readSymbolicLink p >>= readLink
          else pure p

type ImgFetcher m = StateT FileMap m

runImgFetcher :: ImgFetcher m a -> m (a, FileMap)
runImgFetcher f = runStateT f emptyFileMap

class HasImage a where
  fetchImage :: (MonadHttp m, MonadCatch m) => Cli -> a -> ImgFetcher m a
  imageSet :: a -> HS.HashSet ImgRef

instance HasImage Image where
  fetchImage cli im =
    (\r -> im {imgRef = r}) <$> getImage (imgUrl im)
      <* liftIO (showProgress cli "Download image file" <* endProgress cli)
  imageSet = maybe HS.empty HS.singleton . imgRef

instance (HasImage a) => HasImage (Maybe a) where
  fetchImage cli = traverse (fetchImage cli)
  imageSet = maybe HS.empty imageSet

instance (HasImage a, ShowId a) => HasImage [a] where
  fetchImage cli = traverseP cli fetchImage
  imageSet = HS.unions . fmap imageSet

instance (HasImage a) => HasImage (WithRaw a) where
  fetchImage cli v = (\nv -> v {wrVal = nv}) <$> fetchImage cli (wrVal v)
  imageSet = imageSet . wrVal

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
                      refImgDigest = ImgDigest (hash (responseBody dat))
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