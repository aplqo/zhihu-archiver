{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Author
  ( Author (..),
    parseAuthor,
    parseAuthorMaybe,
    poAuthor,
    poAuthorMaybe,
  )
where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.RawParser.TH
import ZhArchiver.RawParser.Util

data Author = Author
  { auId, auUrlToken, auName, auHeadline :: Text,
    auAvatar :: Image
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Author

deriveHasImage ''Author ['auAvatar]

parseAuthor :: JSON.Value -> Parser Author
parseAuthor =
  $( rawParser
       'Author
       [ ('auId, FoParse "id" PoStock),
         ('auUrlToken, FoParse "url_token" PoStock),
         ('auName, FoParse "name" PoStock),
         ('auHeadline, FoParse "headline" PoStock),
         ('auAvatar, FoParse "avatar_url" poImage)
       ]
   )

parseAuthorMaybe :: JSON.Value -> Parser (Maybe Author)
parseAuthorMaybe = fmap (unlessMaybe ((== "0") . auId)) . parseAuthor

poAuthor :: ParseOpt
poAuthor = PoBind [|parseAuthor|]

poAuthorMaybe :: ParseOpt
poAuthorMaybe = PoBind [|parseAuthorMaybe|]