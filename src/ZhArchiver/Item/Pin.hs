{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Pin (PinContent (..), PinBody (..), IId (..), Pin (..)) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Bifunctor
import Data.Int (Int64)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (listE)
import Network.HTTP.Req
import Text.Pandoc
import Text.Pandoc.Builder
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Raw.Parser.Util
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

deriveFromJSON
  defaultOptions
    { fieldLabelModifier = drop 2,
      constructorTagModifier = drop 2
    }
  ''PinContent
deriveToJSON
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2,
      constructorTagModifier = camelTo2 '_' . drop 2
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

instance FromRaw PinContent where
  parseRaw =
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

convertPinContent :: (PandocMonad m) => FilePath -> PinContent -> m Blocks
convertPinContent p c =
  case c of
    PcText t -> (\(Pandoc _ b) -> fromList b) <$> contentToPandoc p t
    PcImage {pcImage = img} -> pure (para (image (imgLocalPath p img) T.empty mempty))
    PcLink {pcLinkTitle = tit, pcUrl = u} -> pure (para (link u tit mempty))
    PcUnknown -> pure (plain (text "unknown pin content"))

data PinBody = PinBody
  { -- | pin id for attaching comment, same as Pin.pinId
    pinId' :: IId Pin,
    pinUpdated :: Time,
    pinReaction :: Int64,
    pinContent :: [PinContent],
    pinOriginPin, pinRepin :: Maybe Pin,
    pinCommentCount :: Int,
    pinComment :: [Comment]
  }

data Pin = Pin
  { pinId :: IId Pin,
    pinAuthor :: Author,
    pinCreated :: Time,
    pinBody :: Maybe PinBody
  }

$( concat
     <$> sequenceA
       [ -- JSON instances
         deriveFromJSON defaultOptions {fieldLabelModifier = drop 3} ''Pin,
         deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''Pin,
         deriveFromJSON defaultOptions {fieldLabelModifier = drop 3} ''PinBody,
         deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''PinBody,
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

instance FromRaw PinBody where
  parseRaw =
    $( rawParser
         'PinBody
         [ ('pinId', foStock "id"),
           ('pinUpdated, FoParse "updated" poTime),
           ('pinReaction, foStock "reaction_count"),
           ('pinContent, FoParse "content" (PoBind [|traverse parseRaw|])),
           ('pinOriginPin, FoParseMaybe "origin_pin" False (PoBind [|parseRaw|])),
           ('pinRepin, FoParseMaybe "repin" False (PoBind [|parseRaw|])),
           ('pinCommentCount, foStock "comment_count"),
           ('pinComment, FoConst (listE []))
         ]
     )

instance Commentable PinBody where
  hasComment p =
    pinCommentCount p /= 0
      || hasComment (pinOriginPin p)
      || hasComment (pinRepin p)
  attachComment cli p@PinBody {pinId' = PinId pid} = do
    (c, rc) <- fetchComment (pushHeader "comment" cli) StPin pid
    (orig, ro) <- attachComment (pushHeader "origin_pin" cli) (pinOriginPin p)
    (rp, r) <- attachComment (pushHeader "repin" cli) (pinRepin p)
    return
      ( p
          { pinOriginPin = orig,
            pinRepin = rp,
            pinComment = c
          },
        fromListRm
          [ ("comment", packLeaf rc),
            ("origin_pin", RtBranch ro),
            ("repin", RtBranch r)
          ]
      )

instance HasContent PinBody where
  convertContent p pb = do
    cont <- mconcat <$> traverse (convertPinContent p) (pinContent pb)
    orig <- convertContent p (pinOriginPin pb)
    return
      ( Just
          ( case orig of
              Just (Pandoc _ bs) ->
                doc
                  ( cont
                      <> blockQuote (para (text "repin:" <> text ((auName . pinAuthor . fromJust . pinOriginPin) pb)) <> fromList bs)
                  )
              Nothing -> doc cont
          )
      )

instance HasContent Pin where
  convertContent p pn = maybe (pure Nothing) (convertContent p) (pinBody pn)

instance ShowId Pin where
  showType = const "pin"
  showId Pin {pinId = PinId p} = T.unpack p

instance ShowName Pin where
  showName = const ""

instance FromRaw Pin where
  parseRaw =
    $( rawParser
         'Pin
         [ ('pinId, foStock "id"),
           ('pinAuthor, foFromRaw "author"),
           ('pinCreated, FoParse "created" poTime),
           ('pinBody, FoCustom [|parseBodyMaybe|])
         ]
     )
    where
      parseBodyMaybe val =
        optional (parseRaw val)

instance ZhData Pin

instance Item Pin where
  newtype IId Pin = PinId {pinIdTxt :: Text}
    deriving newtype (Show, FromJSON, ToJSON)
  type Signer Pin = ()
  fetchRaw _ (PinId pid) =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "pins" /: pid)
        NoReqBody
        jsonResponse
        mempty

deriving instance (Show PinBody)

deriving instance (Show Pin)

instance Commentable Pin where
  hasComment p = hasComment (pinBody p)
  attachComment cli p = first (\b -> p {pinBody = b}) <$> attachComment cli (pinBody p)
