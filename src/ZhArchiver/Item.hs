{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item
  ( RawData (..),
    ZhData (..),
    Item (..),
    ItemContainer (..),
  )
where

import Control.Monad.Catch
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Network.HTTP.Req
import ZhArchiver.Progress (Cli)

newtype RawData a = Raw {unRaw :: JSON.Value}
  deriving (Show)

class ZhData d where
  parseRaw :: RawData d -> Parser d

class (ZhData a) => Item a where
  type IId a
  type Signer a
  fetchRaw :: (MonadHttp m, MonadThrow m) => Signer a -> IId a -> m (RawData a)

class (Item a, ZhData i) => ItemContainer a i where
  type ICOpt a i
  type ICSigner a i
  fetchItemsRaw :: (MonadHttp m, MonadThrow m) => Cli -> ICOpt a i -> ICSigner a i -> a -> m [RawData i]