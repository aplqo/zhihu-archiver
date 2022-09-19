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

encodeFilePretty :: (ToJSON a) => FilePath -> a -> IO ()
encodeFilePretty p a =
  LBS.writeFile
    p
    ( encodePretty' defConfig {confIndent = Spaces 2} a
    )

type Decoder = ExceptT String IO

decodeFile :: (FromJSON a) => FilePath -> Decoder a
decodeFile p = liftIO (eitherDecodeFileStrict p) >>= liftEither
