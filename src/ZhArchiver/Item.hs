{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item (RawData (..), Item (..), Commentable (..)) where

import Control.Monad.Catch
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Network.HTTP.Req

newtype RawData a = Raw {unRaw :: JSON.Value}
  deriving (Show)

class Item a where
  type IId a
  type Signer a
  fetchRaw :: (MonadHttp m, MonadThrow m) => Signer a -> IId a -> m (RawData a)
  parseRaw :: RawData a -> Parser a

class Commentable a where
  commentCount :: a -> Int
  attachComment :: (MonadHttp m, MonadThrow m) => a -> m a
