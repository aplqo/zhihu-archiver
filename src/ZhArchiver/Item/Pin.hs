{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Pin (PinContent (..), Pin (..)) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (listE)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.Progress
import ZhArchiver.RawParser.TH
import ZhArchiver.RawParser.Util
import ZhArchiver.Types

data PinContent
  = PcText Content
  | PcImage
      { pcOriginal, pcImage :: Image
      }
  | PcLink
      { pcLinkTitle :: Text,
        pcUrl :: Text,
        pcLinkImage :: Maybe Image
      }
  | PcUnknown
  deriving (Show)

deriveJSON
  defaultOptions
    { fieldLabelModifier = drop 2,
      constructorTagModifier = drop 2
    }
  ''PinContent

instance ShowId PinContent where
  showType = const "pin-content"
  showId = const ""

instance HasImage PinContent where
  fetchImage cli (PcText c) = PcText <$> fetchImage (pushHeader "text" cli) c
  fetchImage cli PcImage {pcOriginal = orig0, pcImage = img0} = do
    orig <- fetchImage (pushHeader "original-img" cli) orig0
    img <- fetchImage (pushHeader "image" cli) img0
    return (PcImage orig img)
  fetchImage cli p@(PcLink {pcLinkImage = i}) =
    (\img -> p {pcLinkImage = img}) <$> fetchImage (pushHeader "link-img" cli) i
  fetchImage _ PcUnknown = pure PcUnknown

parsePinContent :: JSON.Value -> Parser PinContent
parsePinContent =
  withObject
    "content"
    ( \o ->
        ((o .: "type") :: Parser Text) >>= \case
          "text" ->
            PcText . contentFromHtml <$> o .: "content"
          "image" -> do
            orig <- imgFromUrl <$> o .: "original_url"
            img <- imgFromUrl <$> o .: "url"
            return PcImage {pcOriginal = orig, pcImage = img}
          "link" -> do
            title <- o .: "title"
            url <- o .: "title"
            img <- appUnless T.null imgFromUrl <$> o .: "image_url"
            return
              PcLink {pcLinkTitle = title, pcUrl = url, pcLinkImage = img}
          _ -> pure PcUnknown
    )

data PinBody = PinBody
  { -- | pin id for attaching comment, same as Pin.pinId
    pinId' :: Text,
    pinUpdated :: Time,
    pinReaction :: Int64,
    pinContent :: [PinContent],
    pinOriginPin, pinRepin :: Maybe Pin,
    pinCommentCount :: Int,
    pinComment :: [Comment]
  }
  deriving (Show)

data Pin = Pin
  { pinId :: Text,
    pinAuthor :: Author,
    pinCreated :: Time,
    pinBody :: Maybe PinBody,
    pinRawData :: JSON.Value
  }
  deriving (Show)

$( concat
     <$> sequenceA
       [ -- JSON instances
         deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Pin,
         deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''PinBody,
         -- HasImage instances
         deriveHasImage
           ''PinBody
           [ ('pinContent, "content"),
             ('pinOriginPin, "origin_pin"),
             ('pinRepin, "repin"),
             ('pinComment, "comment")
           ],
         deriveHasImage
           ''Pin
           [('pinAuthor, "author"), ('pinBody, "body")]
       ]
 )

parseBody :: Value -> Parser PinBody
parseBody =
  $( rawParser
       'PinBody
       [ ('pinId', foStock "id"),
         ('pinUpdated, FoParse "updated" poTime),
         ('pinReaction, foStock "reaction_count"),
         ('pinContent, FoParse "content" (PoBind [|traverse parsePinContent|])),
         ('pinOriginPin, FoParseMaybe "origin_pin" False (PoBind [|parseRaw . Raw|])),
         ('pinRepin, FoParseMaybe "repin" False (PoBind [|parseRaw . Raw|])),
         ('pinCommentCount, foStock "comment_count"),
         ('pinComment, FoConst (listE []))
       ]
   )

instance Commentable PinBody where
  hasComment p =
    pinCommentCount p /= 0
      || hasComment (pinOriginPin p)
      || hasComment (pinRepin p)
  attachComment cli p = do
    c <- fetchComment (pushHeader "comment" cli) StPin (pinId' p)
    orig <- attachComment (pushHeader "origin_pin" cli) (pinOriginPin p)
    rp <- attachComment (pushHeader "repin" cli) (pinRepin p)
    return
      p
        { pinOriginPin = orig,
          pinRepin = rp,
          pinComment = c
        }

instance ShowId Pin where
  showType = const "pin"
  showId Pin {pinId = p} = T.unpack p

instance ZhData Pin where
  parseRaw (Raw v) =
    $( rawParser
         'Pin
         [ ('pinId, foStock "id"),
           ('pinAuthor, FoParse "author" poAuthor),
           ('pinCreated, FoParse "created" poTime),
           ('pinBody, FoCustom [|parseBodyMaybe|]),
           ('pinRawData, FoRaw)
         ]
     )
      v
    where
      parseBodyMaybe val =
        withObject "pin" (.:? "is_deleted") v >>= \d ->
          if fromMaybe False d
            then pure Nothing
            else Just <$> parseBody val

instance Item Pin where
  type IId Pin = Text
  type Signer Pin = ()
  fetchRaw _ pid =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "pins" /: pid)
        NoReqBody
        jsonResponse
        mempty

instance Commentable Pin where
  hasComment p = hasComment (pinBody p)
  attachComment cli p = (\b -> p {pinBody = b}) <$> attachComment cli (pinBody p)
