{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calamine.Http (Url, http) where

import qualified Control.Exception as E
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Network (withSocketsDo)
import           Network.HTTP.Client (HttpException(StatusCodeException))
import           Network.HTTP.Client (Request, Manager, parseUrl, withManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types (Status(..))

------------------------------------------------------------------------

type Url = Text

------------------------------------------------------------------------

http :: (MonadIO m, MonadError String m)
     => Url
     -> (Request -> Manager -> IO a)
     -> m a
http url handler = do
    result <- liftIO (safeHttp url handler)
    either throwError return result

safeHttp :: Url -> (Request -> Manager -> IO a) -> IO (Either String a)
safeHttp url handler = unsafeHttp url handler
    `E.catch` handleHttpError url
    `E.catch` (handleAnyError url :: E.SomeException -> IO (Either String b))

unsafeHttp :: Url -> (Request -> Manager -> IO a) -> IO (Either err a)
unsafeHttp url handler = do
    request <- parseUrl (T.unpack url)
    result  <- withSocketsDo (withManager tlsManagerSettings (handler request))
    return (Right result)

------------------------------------------------------------------------

handleHttpError :: Url -> HttpException -> IO (Either String b)
handleHttpError url ex = case ex of
    StatusCodeException (Status _ err) hs _ -> return (Left (detailsOf err hs))
    _                                       -> handleAnyError url ex
  where
    detailsOf err headers = case lookup "X-Response-Body-Start" headers of
      Just msg | not (B.null msg) -> B.unpack msg
      _                           -> B.unpack err

handleAnyError :: E.Exception e => Url -> e -> IO (Either String b)
handleAnyError url ex = return . Left $
    "failed with '" ++ show ex ++ "' when sending request to\n" ++
    "    <" ++ T.unpack url ++ ">"
