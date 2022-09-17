{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Content
  ( Html (..),
    poHtml,
    poMaybeHtml,
    Content (..),
    poContent,
    poContentMaybe,
  )
where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
import ZhArchiver.Image
import ZhArchiver.RawParser.TH
import ZhArchiver.RawParser.Util

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
  fetchImage c =
    (\im -> c {contImages = im})
      <$> getHtmlImages (htmlText (contHtml c))

fromHtml :: Text -> Content
fromHtml u = Content {contHtml = Html u, contImages = emptyImgMap}

poContent :: ParseOpt
poContent = PoMap [|fromHtml|]

poContentMaybe :: ParseOpt
poContentMaybe = PoMap [|appUnless T.null fromHtml|]