{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item
  ( RawData (..),
    encodeFilePretty,
    withDirectory,
    ZhData (..),
    runParser,
    Item (..),
    ItemContainer (..),
    fetchItems,
  )
where

import Control.Monad.Catch
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Req
import System.Directory
import ZhArchiver.Progress

newtype RawData a = Raw {unRaw :: JSON.Value}
  deriving (Show)

encodeFilePretty :: (ToJSON a) => FilePath -> a -> IO ()
encodeFilePretty p a =
  LBS.writeFile
    p
    ( encodePretty' defConfig {confIndent = Spaces 2} a
    )

withDirectory :: FilePath -> IO a -> IO a
withDirectory p a =
  createDirectoryIfMissing True p
    >> withCurrentDirectory p a

class ZhData d where
  parseRaw :: RawData d -> Parser d
  saveData :: FilePath -> d -> IO ()

runParser :: (ZhData a) => RawData a -> a
runParser v =
  case parse parseRaw v of
    Success r -> r
    Error e -> error ("Parse error:" ++ e)

class (ZhData a) => Item a where
  type IId a
  type Signer a
  fetchRaw :: (MonadHttp m, MonadThrow m) => Signer a -> IId a -> m (RawData a)

class (Item a, ZhData i) => ItemContainer a i where
  type ICOpt a i
  type ICSigner a i
  fetchItemsRaw :: (MonadHttp m, MonadThrow m) => Cli -> ICOpt a i -> ICSigner a i -> a -> m [RawData i]
  saveItems :: FilePath -> ICOpt a i -> a -> [i] -> IO ()

fetchItems ::
  (ItemContainer a i, MonadHttp m, MonadThrow m, ShowId a) =>
  Cli ->
  ICOpt a i ->
  ICSigner a i ->
  a ->
  m [i]
fetchItems cli opt sig v =
  fmap runParser
    <$> fetchItemsRaw (pushHeader (showValId v) cli) opt sig v