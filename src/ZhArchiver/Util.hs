module ZhArchiver.Util
  ( encodeFilePretty,
    Decoder,
    decodeFile,
  )
where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LBS
import System.Directory

encodeFilePretty :: (ToJSON a) => Bool -> FilePath -> a -> IO ()
encodeFilePretty overwrite p a =
  unless
    overwrite
    ( doesFileExist p
        >>= (`when` error (p ++ " is exist. Not allowed to overwrite file"))
    )
    >> LBS.writeFile
      p
      ( encodePretty' defConfig {confIndent = Spaces 2} a
      )

type Decoder = ExceptT String IO

decodeFile :: (FromJSON a) => FilePath -> Decoder a
decodeFile p = liftIO (eitherDecodeFileStrict p) >>= liftEither
