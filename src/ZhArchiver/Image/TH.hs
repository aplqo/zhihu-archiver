{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.Image.TH (deriveHasImage) where

import Language.Haskell.TH
import ZhArchiver.Image

deriveHasImage :: Name -> [Name] -> DecsQ
deriveHasImage n [] =
  do
    reportWarning "No field is a instance of HasImage"
    [d|
      instance HasImage $(pure $ ConT n) where
        fetchImage = pure
      |]
deriveHasImage n fields = do
  orig <- newName "orig"
  (stmts, expr) <- unzip <$> traverse (procField orig) fields
  [d|
    instance HasImage $(pure $ ConT n) where
      fetchImage $(pure $ VarP orig) =
        $( pure
             ( DoE Nothing (stmts ++ [NoBindS (AppE (VarE 'return) (RecUpdE (VarE orig) expr))])
             )
         )
    |]
  where
    procField :: Name -> Name -> Q (Stmt, FieldExp)
    procField v fn =
      ( \tp ->
          ( BindS
              (VarP tp)
              (AppE (VarE 'fetchImage) (AppE (VarE fn) (VarE v))),
            (fn, VarE tp)
          )
      )
        <$> newName "f"