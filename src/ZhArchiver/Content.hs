{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Content
  ( Html (..),
    poHtml,
    poMaybeHtml,
    Content (..),
    contentFromHtml,
    contentToPandoc,
    poContent,
    poContentMaybe,
    HasContent (..),
  )
where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Default
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Walk
import ZhArchiver.Image hiding (Image)
import ZhArchiver.Raw
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Raw.Parser.Util

newtype Html = Html {htmlText :: Text}
  deriving (Show)
  deriving newtype (FromJSON, ToJSON)

poHtml :: ParseOpt
poHtml = PoMap [|Html|]

poMaybeHtml :: ParseOpt
poMaybeHtml = PoMap [|appIf (not . T.null) Html|]

data Content = Content
  { contHtml :: Html,
    contImages :: Maybe ImgMap
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4} ''Content

instance HasImage Content where
  fetchImage cli c =
    (\im -> c {contImages = Just im})
      <$> getHtmlImages cli (htmlText (contHtml c))
  imageSet = maybe HS.empty elemImages . contImages

contentFromHtml :: Text -> Content
contentFromHtml u = Content {contHtml = Html u, contImages = Nothing}

poContent :: ParseOpt
poContent = PoMap [|contentFromHtml|]

poContentMaybe :: ParseOpt
poContentMaybe = PoMap [|appUnless T.null contentFromHtml|]

class HasContent a where
  convertContent :: (PandocMonad m) => FilePath -> a -> m (Maybe Pandoc)

instance (HasContent a) => HasContent (Maybe a) where
  convertContent p = fmap join . traverse (convertContent p)

instance (HasContent a) => HasContent (WithRaw a) where
  convertContent p = convertContent p . wrVal

contentToPandoc :: PandocMonad m => FilePath -> Content -> m Pandoc
contentToPandoc store Content {contHtml = Html h, contImages = img} =
  walk (walk procImage :: Inline -> Inline) <$> readHtml def h
  where
    mp = fromMaybe emptyImgMap img
    procImage :: Inline -> Inline
    procImage =
      \case
        Image a t (url, tit) -> Image a t (lookupLocalPath store mp url, tit)
        a -> a