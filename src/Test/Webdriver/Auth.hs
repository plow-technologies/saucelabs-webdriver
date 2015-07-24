{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Webdriver.Auth where

import           Control.Monad.Catch          (MonadCatch, MonadThrow)
import           Control.Monad.Reader
import           Control.Monad.State.Strict   (MonadState, StateT, evalStateT,
                                               get, put)
import qualified Control.Monad.State.Strict   as S
import           Control.Monad.Trans.Control

import           Test.WebDriver
import           Test.WebDriver.Capabilities  (defaultCaps)
import           Test.WebDriver.Class
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Config        (defaultConfig, mkSession)
import           Test.WebDriver.JSON          (single)
import           Test.WebDriver.Session

import           Data.Aeson
import           Data.Aeson.Types             (Parser, typeMismatch)
import           Network.HTTP.Types           (methodPut)
import           Network.HTTP.Types.Header

import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   (ByteString)
import           Data.ByteString.Lazy.Char8   as LBS (fromStrict, length, null,
                                                      pack, unpack)
import qualified Data.Maybe                   as DM
import           Data.Text                    as T (Text, breakOn, null,
                                                    splitOn)
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Lazy.Encoding      as TLE

import           Control.Applicative
import           Control.Exception            (SomeException, toException)
import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

import           Data.Typeable                (Typeable)
import           Data.Word                    (Word, Word8)

import           Network.HTTP.Client
import qualified Network.HTTP.Conduit         as HC

import           Data.Either.Unwrap           (fromRight)
import qualified Test.Webdriver.Auth.Internal as IN

newtype WDAuth a = WDAuth {
   unWDAuth :: StateT (WDSession, Request -> Request) IO a
   } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

-- newtype WDAuth a = WDAuth (StateT (WDSession, Request -> Request) IO a)
--     deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadBase IO WDAuth where
  liftBase = WDAuth . liftBase

#if MIN_VERSION_monad_control(1,0,0)

--This is the newer way of writing a MonadBaseControl instance
instance MonadBaseControl IO WDAuth where
  type StM WDAuth a = StM (StateT (WDSession, Request -> Request) IO) a
  liftBaseWith f = WDAuth $
    liftBaseWith $ \runInBase ->
    f $ (\(WDAuth sT) -> runInBase $ sT)
  restoreM = WDAuth . restoreM
#else
instance MonadBaseControl IO WDAuth where
  newtype StM WDAuth a = ST {unST :: StM (StateT (WDSession, Request -> Request) IO) a}
  liftBaseWith f = WDAuth . liftBaseWith $ \runInBase ->
        f (liftM ST . runInBase . unWDAuth)
  restoreM = WDAuth . restoreM . unST
#endif
instance WDSessionState WDAuth where
  getSession = WDAuth $ S.StateT (\v@(sess, _) -> return (sess,v))
  putSession session = WDAuth $ S.StateT (\(_,addAuth) -> return ( () , (session, addAuth) ) )

instance WebDriver WDAuth where
  doCommand headers method path args = do
    applyAuth <- getApplyAuth
    IN.mkRequestWith applyAuth headers method path args
    >>= IN.sendHTTPRequest
    >>= IN.getJSONResult
    >>= either throwIO return

--Roughly the same as the function in Data.Either.Unwrap also called fromRight
--With added error handling for incorrect jobId format
jobIdFromRight :: Either e a -> a
jobIdFromRight (Right a) = a
jobIdFromRight (Left _) = error "Bad Job Id format"

--Queries SauceLabs for the most recent Job Id
getJobId :: String -> String -> IO (Either HttpException (Response ByteString))
getJobId user pswd = runResourceT $ do
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/" ++ user ++ "/jobs?limit=1")
    let req = applyBasicAuth (BS.pack user) (BS.pack pswd) $ tReq {HC.method = methodGet, HC.requestBody = HC.RequestBodyBS body}
    resp <- liftIO $ try (HC.withManager ( HC.httpLbs req)) :: ResourceT IO (Either HC.HttpException (HC.Response ByteString))
    return resp
    where
      body = BS.pack "" --Empty request body for GET request

--Turns the Job Id response into a String, checking to make sure getJobId returned a Right Response ByteString
getJobIdString :: String -> String -> IO (Either SomeException String)
getJobIdString user pswd = do
  jobId <- liftIO $ getJobId user pswd
  case jobId of
    Left _  -> do return $ error "Error occured receiving job id from Sauce Labs"
    Right _ -> do return $ checkJobIdFormat $ unpack $ responseBody $ jobIdFromRight $ jobId

--Checks format of Job id, fixing it recursively or throwing an exception.
checkJobIdFormat :: String -> Either SomeException String
checkJobIdFormat input
  | chars == 44 = checkJobIdFormat (take 32 $ drop 9 $ input)
  | chars == 32 = Right input
  | otherwise   = Left (error "Bad Job Id format")
  where chars = Prelude.length input

--Sends a Passed status to the most recent Job on SauceLabs
sendPassed :: String -> String ->  IO (Either HttpException (Response ByteString))
sendPassed user pswd = runResourceT $ do
    jobIdString <- liftIO $ getJobIdString user pswd
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/" ++ user ++ "/jobs/" ++ (jobIdFromRight jobIdString))
    let req = applyBasicAuth (BS.pack user) (BS.pack pswd) $ tReq {HC.method = methodPut, HC.requestHeaders = contentHeader, HC.requestBody = HC.RequestBodyBS body}
    resp <- liftIO $ try (HC.withManager ( HC.httpLbs req)) :: ResourceT IO (Either HC.HttpException (HC.Response ByteString))
    return resp
    where
      body = BS.pack "{\"passed\": true}"
      contentHeader = [(hContentType, (BS.pack "application/json"))]

--Sends a Failed status to the most recent Job on SauceLabs
sendFailed :: String -> String -> IO (Either HttpException (Response ByteString))
sendFailed user pswd = runResourceT $ do
    jobIdString <- liftIO $ getJobIdString user pswd
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/" ++ user ++ "/jobs/" ++ (jobIdFromRight jobIdString))
    let req = applyBasicAuth (BS.pack user) (BS.pack pswd) $ tReq {HC.method = methodPut, HC.requestHeaders = contentHeader, HC.requestBody = HC.RequestBodyBS body}
    resp <- liftIO $ try (HC.withManager ( HC.httpLbs req)) :: ResourceT IO (Either HC.HttpException (HC.Response ByteString))
    return resp
    where
      body = BS.pack "{\"passed\": false}"
      contentHeader = [(hContentType, (BS.pack "application/json"))]

--Default session configuration for pointing to Sauce Labs, the brwsr and version
--arguments allow you to choose which browser to run the test in from the main function.
--Accepted browser arguments include chrome, firefox, and ie
--versions are Ints, think ie 9, chrome 43, firefox 38
sauceConfig :: Browser -> Int -> WDConfig
sauceConfig brwsr vers = defaultConfig { wdHost = "ondemand.saucelabs.com"
                                       , wdCapabilities = defaultCaps { browser = brwsr
                                                                      , version = Just $ show vers
                                                                      }
                                       }

getApplyAuth :: WDAuth (Request -> Request)
getApplyAuth = WDAuth $ S.StateT (\v@(_, addAuth) -> return (addAuth, v))

runWDAuth :: BS.ByteString -> BS.ByteString -> WDSession -> WDAuth a -> IO a
runWDAuth user pass sess (WDAuth wd)
  = evalStateT wd (sess, applyBasicAuth user pass)

runWDAuthWith :: Browser -> Int -> BS.ByteString -> BS.ByteString -> WDAuth a -> IO a
runWDAuthWith brwsr vers user pass wd = do
  sess <- mkSession $ sauceConfig brwsr vers
  runWDAuth user pass sess $ createSession (wdRequestHeaders $ sauceConfig brwsr vers) (wdCapabilities $ sauceConfig brwsr vers) >> wd

--Checks that the lastResult is good, sends a pass/fail to SauceLabs,
--and closes the session.
checkPassed :: String -> String -> WDAuth (Either IN.FailedCommand Text) -> WDAuth (Either IN.FailedCommand Text)
checkPassed user pswd test = do
  lastResult <- test
  case lastResult of
    Left e  -> do _ <- liftIO $ sendFailed user pswd
                  closeSession
                  return $ Left (e :: IN.FailedCommand)

    Right _ -> do _ <- liftIO $ sendPassed user pswd
                  closeSession
                  return $ Right "Test Successful"
