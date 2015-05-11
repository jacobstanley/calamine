{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Concurrent.STM (TChan, atomically)
import           Control.Concurrent.STM (newTChanIO, writeTChan, readTChan)
import qualified Control.Exception as E
import           Control.Monad (void, forever)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Except (MonadError, throwError, catchError)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client (Response(..), Request(..), httpLbs)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.IO (hFlush, stderr)
import           System.IO.Unsafe (unsafePerformIO)

import           Calamine.Http (Url, http)

------------------------------------------------------------------------

main :: IO ()
main = do
    forkLogging
    putStrLn "Calamine"
    rs <- mapConcurrently (runCalamine . get . mavenUrl) deps
    mapM_ summary rs
  where
    summary (Left err) = putStrLn err
    summary (Right bs) = do
      let sz = show (L.length bs `div` 1024)
      putStrLn ("Size = " <> sz <> "K")

------------------------------------------------------------------------

logChan :: TChan Text
logChan = unsafePerformIO newTChanIO

forkLogging :: IO ()
forkLogging = void . forkIO . forever $ do
    msg <- atomically (readTChan logChan)
    T.hPutStrLn stderr msg
    hFlush stderr

writeLog :: Text -> IO ()
writeLog msg = atomically (writeTChan logChan msg)

------------------------------------------------------------------------

type Calamine = ExceptT String IO

runCalamine :: Calamine a -> IO (Either String a)
runCalamine = runExceptT

------------------------------------------------------------------------

get :: (MonadIO m, MonadError String m) => Url -> m L.ByteString
get url =
    read `catchError` const (cache =<< fetch)
  where
    read     = readCache url
    cache bs = writeCache url bs >> return bs
    fetch    = responseBody <$> httpGet url

httpGet :: (MonadIO m, MonadError String m) => Url -> m (Response L.ByteString)
httpGet url = http url $ \request manager -> do
    writeLog ("Fetching " <> url)
    httpLbs (go request) manager
  where
    go r = r { requestHeaders = [("User-Agent", "calamine")] }

------------------------------------------------------------------------

deps :: [(Group, Artifact, Revision)]
deps = [ ("org.scala-lang", "scala-library", "2.11.0")
       , ("org.scala-lang", "scala-library", "2.11.1")
       , ("org.scala-lang", "scala-library", "2.11.2")
       , ("org.scala-lang", "scala-library", "2.11.3")
       , ("org.scala-lang", "scala-library", "2.11.4")
       , ("org.scala-lang", "scala-library", "2.11.5")
       , ("org.scala-lang", "scala-library", "2.11.6")
       ]

------------------------------------------------------------------------

type Group    = Text
type Artifact = Text
type Revision = Text

mavenUrl :: (Group, Artifact, Revision) -> Url
mavenUrl (grp, art, rev) = "https://repo1.maven.org/maven2/"
                        <> grp' <> "/"
                        <> art  <> "/"
                        <> rev  <> "/"
                        <> art  <> "-"
                        <> rev  <> ".jar"
  where
    grp' = T.replace "." "/" grp

------------------------------------------------------------------------

readCache :: (MonadIO m, MonadError String m) => Url -> m L.ByteString
readCache url = do
    path <- cacheLocationOf url
    runIO (L.readFile path)

writeCache :: (MonadIO m, MonadError String m) => Url -> L.ByteString -> m ()
writeCache url bs = do
    path <- cacheLocationOf url
    runIO $ do
      createDirectoryIfMissing True (takeDirectory path)
      L.writeFile path bs

cacheLocationOf :: MonadError String m => Url -> m FilePath
cacheLocationOf url
    | https `T.isPrefixOf` url = dropPrefix https url
    | http  `T.isPrefixOf` url = dropPrefix http  url
    | otherwise                = throwError ("invalid url: " <> T.unpack url)
  where
    https = "https://"
    http  = "http://"

    dropPrefix pfx = return
                   . T.unpack
                   . (".cache/" <>)
                   . T.drop (T.length pfx)

------------------------------------------------------------------------

runIO :: (MonadIO m, MonadError String m) => IO a -> m a
runIO io = do
    result <- liftIO (tryIO io)
    either throwError return result

tryIO :: IO a -> IO (Either String a)
tryIO = E.tryJust convert
  where
    convert :: E.IOException -> Maybe String
    convert = Just . E.displayException
