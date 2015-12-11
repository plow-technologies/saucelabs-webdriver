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
import qualified Test.WebDriver.Capabilities  as WD (defaultCaps, proxy)
import           Test.WebDriver.Class
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Config        (defaultConfig, mkSession)
import           Test.WebDriver.JSON          (single)
import           Test.WebDriver.Session

import           Data.Aeson
import           Data.Aeson.Types             (Pair, Parser, emptyObject,
                                               typeMismatch)
import           Network.HTTP.Types           (methodPut)
import           Network.HTTP.Types.Header

import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   (ByteString)
import           Data.ByteString.Lazy.Char8   as LBS (append, fromStrict,
                                                      length, null, pack,
                                                      unpack)
import qualified Data.Maybe                   as DM
import           Data.Text                    as T (Text, breakOn, null,
                                                    splitOn, pack, unpack)
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Lazy.Encoding      as TLE

import           Control.Applicative hiding (empty)
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
import           Control.Lens hiding ((.=))
import qualified Control.Lens.Reified as RG
import           Data.Aeson.Lens
import qualified Data.Map.Strict as M

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

data Job = Job {name :: Text, jobId :: Text} deriving (Show, Eq)


getJobs :: String -> String -> IO (Either HttpException (Response ByteString))
getJobs user pswd = runResourceT $ do
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/" ++ user ++ "/jobs?limit=10&full=:get_full_info")
    let req = applyBasicAuth (BS.pack user) (BS.pack pswd) $ tReq {HC.method = methodGet, HC.requestBody = HC.RequestBodyBS body}
    resp <- liftIO $ try (HC.withManager ( HC.httpLbs req)) :: ResourceT IO (Either HC.HttpException (HC.Response ByteString))
    return resp
    where
      body = BS.pack "" --Empty request body for GET request

--Turns the Job Id response into a String, checking to make sure getJobId returned a Right Response ByteString
getJobIdStringByName :: String -> String -> String -> IO (Either String String)
getJobIdStringByName user pswd testName = do
  rslt <- getJobs user pswd
  case rslt of
    Left e     -> return $ Left $ show e
          
    Right resp -> do
      let body = responseBody resp
          value = decode body :: Maybe Value
      case value of
        Nothing  -> return $ Left "Failed to decode HTTP response"
        Just val -> do 
          let (jobMap, jobList) = mapNamesToJobIds $ val
              jobId = (M.lookup (T.pack testName) jobMap) :: Maybe Text
          --putStrLn $ show jobMap
          --putStrLn $ show jobList
          case jobId of
            Nothing -> return $ Left "Failed to find job id by name"
            Just txt -> return $ Right $ T.unpack txt

mapNamesToJobIds :: Value -> (M.Map Text Text, [(Text, Text)])
mapNamesToJobIds v = (M.fromList $ reverse testNames, testNames)
  where
    testNames = v ^.. values.runFold (buildJob <$> 
                                         (Fold (key "name" . _String)) <*>
                                         (Fold (key "id" . _String)))
    buildJob n k = (n, k)                                        

--Sends a Passed status to the job with the same test name on SauceLabs
sendPassed :: String -> String -> String -> IO (Either HttpException (Response ByteString))
sendPassed user pswd testName = runResourceT $ do
    mjobIdString <- liftIO $ getJobIdStringByName user pswd testName
    case mjobIdString of
      Left txt          -> error txt
      Right jobIdString -> do
        tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/" ++ user ++ "/jobs/" ++ jobIdString)
        let req = applyBasicAuth (BS.pack user) (BS.pack pswd) $ tReq {HC.method = methodPut, HC.requestHeaders = contentHeader, HC.requestBody = HC.RequestBodyBS body}
        resp <- liftIO $ try (HC.withManager ( HC.httpLbs req)) :: ResourceT IO (Either HC.HttpException (HC.Response ByteString))
        return resp
        where
          body = BS.pack "{\"passed\": true}"
          contentHeader = [(hContentType, (BS.pack "application/json"))]

--Sends a Failed status to the Job based on the test name on SauceLabs
sendFailed :: String -> String -> String -> IO (Either HttpException (Response ByteString))
sendFailed user pswd testName = runResourceT $ do
    mjobIdString <- liftIO $ getJobIdStringByName user pswd testName
    case mjobIdString of
      Left txt          -> error txt
      Right jobIdString -> do
        tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/" ++ user ++ "/jobs/" ++ jobIdString)
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
sauceConfig :: Browser -> Int -> Text -> WDConfig
sauceConfig brwsr vers testName =
                      defaultConfig { wdHost = "ondemand.saucelabs.com"
                                    , wdCapabilities = WD.defaultCaps { browser = brwsr
                                                                      , version = Just $ show vers
                                                                      , platform = Vista
                                                                      , additionalCaps = [ setName testName
                                                                                         , setTeam
                                                                                         ]
                                                                      }
                                    }
ffDriverVersion:: Pair
ffDriverVersion = ("seleniumVersion", String "2.48.2")

chrDriverVersion:: Pair
chrDriverVersion = ("chromedriverVersion", String "2.20")

ieDriverVersion :: Pair
ieDriverVersion = ("iedriverVersion", String "2.48.0")

setTeam :: Pair
setTeam = ("public", String "team")

setName :: Text -> Pair
setName name = ("name", String name)

getApplyAuth :: WDAuth (Request -> Request)
getApplyAuth = WDAuth $ S.StateT (\v@(_, addAuth) -> return (addAuth, v))

runWDAuth :: BS.ByteString -> BS.ByteString -> WDSession -> WDAuth a -> IO a
runWDAuth user pass sess (WDAuth wd) = do
  evalStateT wd (sess, applyBasicAuth user pass)

runWDAuthWith :: Browser -> Int -> Text -> BS.ByteString -> BS.ByteString -> WDAuth a -> IO a
runWDAuthWith brwsr vers name user pass wd = do
  sess <- mkSession $ sauceConfig brwsr vers name
  runWDAuth user pass sess $ createSession (wdRequestHeaders $ sauceConfig brwsr vers name) (wdCapabilities $ sauceConfig brwsr vers name) >> wd

--Checks that the lastResult is good, sends a pass/fail to SauceLabs,
--and closes the session.
checkPassed :: String -> String -> Either IN.FailedCommand Text -> String -> IO (Either IN.FailedCommand Text)
checkPassed user pswd lastResult testName =
  case lastResult of
    Left e  -> do _ <- sendFailed user pswd testName
                  return $ Left (e :: IN.FailedCommand)

    Right _ -> do _ <- sendPassed user pswd testName
                  return $ Right "Test Successful"
