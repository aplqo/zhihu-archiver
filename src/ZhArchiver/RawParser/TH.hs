{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.RawParser.TH
  ( ParseOpt (..),
    FieldOpt (..),
    foStock,
    rawParser,
  )
where

import Data.Aeson
import Data.Bifunctor
import Data.Maybe
import Language.Haskell.TH

data ParseOpt
  = PoStock
  | PoMap ExpQ
  | PoBind ExpQ
  | PoCustom ExpQ

data FieldOpt
  = FoRaw
  | FoParse String ParseOpt
  | FoConst ExpQ

foStock :: String -> FieldOpt
foStock s = FoParse s PoStock

rawParser :: Name -> [(Name, FieldOpt)] -> ExpQ
rawParser typ fs = do
  orig <- newName "val"
  obj <- newName "obj"
  (bnd, con) <- bimap catMaybes catMaybes . unzip <$> traverse (procField orig obj) fs
  let lam =
        LamE
          [VarP obj]
          ( DoE
              Nothing
              ( bnd
                  ++ [NoBindS (VarE 'return `AppE` RecConE typ con)]
              )
          )
  return
    ( LamE
        [VarP orig]
        ( VarE 'withObject `AppE` LitE (StringL (show typ)) `AppE` lam `AppE` VarE orig
        )
    )
  where
    procField orig _ (n, FoRaw) = pure (Nothing, Just (n, VarE orig))
    procField _ _ (n, FoConst e) = (\c -> (Nothing, Just (n, c))) <$> e
    procField _ obj (n, FoParse pat opt) =
      do
        recFld <- newName "recFld"
        let fld = (VarE '(.:) `AppE` VarE obj) `AppE` LitE (StringL pat)
        bnd <-
          case opt of
            PoStock -> pure fld
            PoMap e -> (\mp -> VarE 'fmap `AppE` mp `AppE` fld) <$> e
            PoBind e -> (\bnd -> VarE '(>>=) `AppE` fld `AppE` bnd) <$> e
            PoCustom e -> (\p -> p `AppE` VarE obj) <$> e
        return (Just (BindS (VarP recFld) bnd), Just (n, VarE recFld))