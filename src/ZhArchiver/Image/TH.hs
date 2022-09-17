{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.Image.TH (deriveHasImage) where

import Language.Haskell.TH
import ZhArchiver.Image
import ZhArchiver.Progress

deriveHasImage :: Name -> [(Name, String)] -> DecsQ
deriveHasImage n [] =
  do
    reportWarning "No field is a instance of HasImage"
    [d|
      instance HasImage $(pure $ ConT n) where
        fetchImage _ = pure
      |]
deriveHasImage n fields = do
  orig <- newName "orig"
  cli <- newName "cli"
  (stmts, expr) <- unzip <$> traverse (procField orig cli) fields
  [d|
    instance HasImage $(pure $ ConT n) where
      fetchImage $(varP cli) $(varP orig) =
        $( pure
             ( DoE Nothing (stmts ++ [NoBindS (AppE (VarE 'return) (RecUpdE (VarE orig) expr))])
             )
         )
    |]
  where
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