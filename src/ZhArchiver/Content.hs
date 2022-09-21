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
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Walk
import ZhArchiver.Image
  ( HasImage (..),
    ImgMap,
    emptyImgMap,
    getHtmlImages,
    lookupLocalPath,
  )
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
    contImages :: ImgMap
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Content

instance HasImage Content where
  fetchImage cli c =
    (\im -> c {contImages = im})
      <$> getHtmlImages cli (htmlText (contHtml c))

contentFromHtml :: Text -> Content
contentFromHtml u = Content {contHtml = Html u, contImages = emptyImgMap}

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
    procImage :: Inline -> Inline
    procImage =
      \case
        Image a t (url, tit) -> Image a t (lookupLocalPath store img url, tit)
        a -> a