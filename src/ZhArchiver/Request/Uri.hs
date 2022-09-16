{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.Request.Uri where

import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.URI
import Text.URI.QQ

https :: RText 'Scheme
https = [scheme|https|]

data PathSeg
  = -- | Fixed
    F String
  | -- | Text
    T
  | -- | PathSeg
    P
  deriving (Eq)

pathTemplate :: [PathSeg] -> ExpQ
pathTemplate ps =
  do
    (a, s, e) <- iter ps
    es <- [e|return ($(return $ head e) :| $(return (ListE (tail e))))|]
    return
      ( LamE
          a
          (DoE Nothing (s ++ [NoBindS es]))
      )
  where
    iter [] = return ([], [], [])
    iter (x : xs) =
      do
        (as, ss, es) <- iter xs
        case x of
          F s ->
            do
              e <- quoteExp pathPiece s
              return (as, ss, e : es)
          T ->
            do
              an <- newName "x"
              pn <- newName "p"
              return
                ( VarP an : as,
                  BindS (VarP pn) (AppE (VarE 'mkPathPiece) (VarE an)) : ss,
                  VarE pn : es
                )
          P ->
            do
              an <- newName "x"
              return
                (VarP an : as, ss, VarE an : es)

apiPath :: String -> String -> ExpQ
apiPath c v = pathTemplate [F "api", F "v4", F c, T, F v]

httpsURI :: NonEmpty (RText 'PathPiece) -> [QueryParam] -> URI
httpsURI pat par =
  URI
    { uriScheme = Just [scheme|https|],
      uriAuthority = Right (Authority Nothing [host|www.zhihu.com|] Nothing),
      uriPath = Just (False, pat),
      uriQuery = par,
      uriFragment = Nothing
    }
