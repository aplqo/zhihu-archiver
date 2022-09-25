{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.Item.Article.Parser (mkArticleParser) where

import Data.Bifunctor (Bifunctor (first))
import Language.Haskell.TH
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Types

mkArticleParser :: Bool -> ExpQ
mkArticleParser hasTitleImg =
  rawParser
    (mkName "Article")
    ( first (\n -> mkName ("ZhArchiver.Item.Article." ++ n))
        <$> [ ("artId", foStock "id"),
              ("artTitle", foStock "title"),
              ("artImage", FoParse "image_url" poImageMaybe),
              ( "artTitleImage",
                if hasTitleImg
                  then FoParse "title_image" poImageMaybe
                  else FoConst [|Nothing|]
              ),
              ("artAuthor", foFromRaw "author"),
              ("artCreate", FoParse "created" poTime),
              ("artUpdate", FoParse "updated" poTime),
              ("artVote", foStock "voteup_count"),
              ("artContent", FoParse "content" poContent),
              ("artCommentCount", foStock "comment_count"),
              ("artComment", FoConst (conE 'Nothing))
            ]
    )
