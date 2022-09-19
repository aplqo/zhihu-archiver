module ZhArchiver.Raw
  ( RawTree (..),
    packLeaf,
    RawMap,
    emptyRm,
    singletonRm,
    fromListRm,
    mergeRm,
    RawData (..),
    FromRaw (..),
    runParser,
    runRawParser,
    WithRaw (..),
    storeRaw,
    attachRaw,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Lazy as LHM
import Data.Typeable
import qualified Data.Vector as V
import System.Directory
import System.FilePath
import ZhArchiver.Progress
import ZhArchiver.Util

newtype RawData a = Raw {unRaw :: JSON.Value}
  deriving (Show)

class FromRaw a where
  parseRaw :: JSON.Value -> Parser a

runParser :: (a -> Parser b) -> a -> b
runParser p v =
  case parse p v of
    Success r -> r
    Error e -> error ("Parse error:" ++ e)

runRawParser :: (JSON.Value -> Parser a) -> RawData a -> a
runRawParser p (Raw v) = runParser p v

data RawTree
  = RtLeaf JSON.Value
  | RtBranch RawMap
  deriving (Show)

packLeaf :: [JSON.Value] -> RawTree
packLeaf v = RtLeaf (JSON.Array (V.fromList v))

type RawMap = LHM.HashMap String RawTree

emptyRm :: RawMap
emptyRm = LHM.empty

singletonRm :: String -> RawTree -> RawMap
singletonRm = LHM.singleton

mergeRm :: RawMap -> RawMap -> RawMap
mergeRm = LHM.unionWith mergeTree
  where
    mergeTree (RtBranch b1) (RtBranch b2) = RtBranch (mergeRm b1 b2)
    mergeTree _ _ = error "merge raw tree failed"

fromListRm :: [(String, RawTree)] -> RawMap
fromListRm = LHM.fromList

data WithRaw a = WithRaw
  { wrVal :: a,
    wrRawData :: Maybe RawMap
  }
  deriving (Show)

instance Functor WithRaw where
  fmap f v = v {wrVal = f (wrVal v)}

instance (ShowId a) => ShowId (WithRaw a) where
  showType = const (showType @a Proxy)
  valType = valType . wrVal
  showId = showId . wrVal

instance (ShowName a) => ShowName (WithRaw a) where
  showName = showName . wrVal

instance (FromRaw a) => FromRaw (WithRaw a) where
  parseRaw v = do
    orig <- parseRaw v
    return
      WithRaw
        { wrVal = orig,
          wrRawData = Just (singletonRm "main" (RtLeaf v))
        }

storeRaw :: FilePath -> WithRaw a -> IO ()
storeRaw _ WithRaw {wrRawData = Nothing} = pure ()
storeRaw p WithRaw {wrRawData = Just rd} =
  withCurrentDirectory p $
    createDirectoryIfMissing False "raw"
      >> withCurrentDirectory "raw" (write rd)
  where
    write hm =
      traverse_
        ( \(k, v) ->
            case v of
              RtLeaf l -> encodeFilePretty (k <.> "json") l
              RtBranch b ->
                unless (LHM.null b) $
                  createDirectoryIfMissing True k
                    >> withCurrentDirectory k (write b)
        )
        (LHM.toList hm)

attachRaw :: FilePath -> a -> Decoder (WithRaw a)
attachRaw p orig =
  (\f -> WithRaw {wrVal = orig, wrRawData = Just f})
    <$> readFs (p </> "raw")
  where
    readFs pat =
      fromListRm
        <$> ( liftIO (listDirectory pat)
                >>= traverse
                  ( \f -> do
                      isDir <- liftIO $ doesDirectoryExist f
                      v <-
                        if isDir
                          then RtBranch <$> readFs f
                          else RtLeaf <$> decodeFile f
                      return (takeBaseName f, v)
                  )
            )