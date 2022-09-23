{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.Image.TH (deriveHasImage) where

import qualified Data.HashSet as HS
import Language.Haskell.TH
import ZhArchiver.Image
import ZhArchiver.Progress

deriveHasImage :: Name -> [(Name, String)] -> DecsQ
deriveHasImage n [] =
  do
    reportWarning "No field is a instance of HasImage"
    [d|
      instance HasImage $(pure $ ConT n) where
        fetchImage = const pure
        imageSet = const HS.empty
      |]
deriveHasImage n fields = do
  (fCli, fOrig, fetch) <- procFetch
  (sOrig, set) <- procSet
  [d|
    instance HasImage $(pure $ ConT n) where
      fetchImage $(varP fCli) $(varP fOrig) = $(pure fetch)
      imageSet $(varP sOrig) = $(pure set)
    |]
  where
    procFetch = do
      orig <- newName "orig"
      cli <- newName "cli"
      (stmts, expr) <- unzip <$> traverse (procField orig cli) fields
      return
        (cli, orig, DoE Nothing (stmts ++ [NoBindS (AppE (VarE 'return) (RecUpdE (VarE orig) expr))]))
    procField :: Name -> Name -> (Name, String) -> Q (Stmt, FieldExp)
    procField v cli (fn, l) =
      ( \tp ->
          ( BindS
              (VarP tp)
              ( VarE 'fetchImage
                  `AppE` (VarE 'pushHeader `AppE` LitE (StringL l) `AppE` VarE cli)
                  `AppE` (VarE fn `AppE` VarE v)
              ),
            (fn, VarE tp)
          )
      )
        <$> newName "f"

    procSet = do
      orig <- newName "orig"
      let fs = ListE (fmap ((\f -> VarE 'imageSet `AppE` (VarE f `AppE` VarE orig)) . fst) fields)
      return (orig, VarE 'HS.unions `AppE` fs)