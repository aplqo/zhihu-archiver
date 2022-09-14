{-# LANGUAGE OverloadedStrings #-}

module ZhArchiver.Request.Zse96V3 (ZseState, setup, zse96) where

import Control.Monad.IO.Class
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Network.HTTP.Client as HNC
import qualified Network.HTTP.Client as NHC
import Network.HTTP.Req
import Network.HTTP.Types.Header
import System.Process

data ZseState = ZseState
  { d_c0 :: ByteString,
    cookies :: NHC.CookieJar,
    encoder :: FilePath
  }

cookieName :: ByteString
cookieName = TE.encodeUtf8 "d_c0"

userAgent :: Option scheme
userAgent = header (TE.encodeUtf8 "User-Agent") (TE.encodeUtf8 "Mozilla/5.0 (X11; Linux x86_64; rv:104.0) Gecko/20100101 Firefox/104.0")

setup :: FilePath -> IO ZseState
setup prog =
  do
    c <-
      runReq defaultHttpConfig $
        responseCookieJar
          <$> req
            GET
            (https "www.zhihu.com" /: "explore")
            NoReqBody
            ignoreResponse
            userAgent
    return
      ZseState
        { d_c0 =
            NHC.cookie_value
              (fromJust $ find (\i -> NHC.cookie_name i == cookieName) (NHC.destroyCookieJar c)),
          cookies = c,
          encoder = prog
        }

zse93Header :: Header
zse93Header = (CI.mk $ TE.encodeUtf8 "x-zse-93", TE.encodeUtf8 "101_3_3.0")

hZse96 :: HeaderName
hZse96 = CI.mk $ TE.encodeUtf8 "x-zse-96"

zse96 :: MonadHttp m => ZseState -> NHC.Request -> m NHC.Request
zse96 zs u =
  do
    let param =
          BSB.toLazyByteString
            ( mconcat
                [ TE.encodeUtf8Builder "101_3_3.0",
                  TE.encodeUtf8Builder "+",
                  BSB.byteString (NHC.path u),
                  BSB.byteString (NHC.queryString u),
                  TE.encodeUtf8Builder "+",
                  BSB.byteString (d_c0 zs)
                ]
            )

    ret <-
      liftIO
        ( TE.encodeUtf8 . T.stripEnd . T.pack
            <$> readProcess
              (encoder zs)
              [ show $
                  hashWith
                    MD5
                    (LBS.toStrict param)
              ]
              ""
        )
    now <- liftIO getCurrentTime

    return
      ( u
          { NHC.requestHeaders =
              HNC.requestHeaders u
                ++ [ (hCookie, fst (NHC.computeCookieString u (cookies zs) now True)), -- (userAgent <> cookieJar (cookies zs) <> zse93Header <> header zse96Name ret)
                     zse93Header,
                     (hZse96, ret)
                   ]
          }
      )