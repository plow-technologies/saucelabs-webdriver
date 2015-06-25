{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Webdriver.Auth where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad.Catch          (MonadCatch, MonadThrow)
import           Control.Monad.Reader
import           Control.Monad.State.Strict   (MonadState, StateT, evalStateT,
                                               get, put)
import qualified Control.Monad.State.Strict   as S
import           Control.Monad.Trans.Control  (MonadBaseControl (..), StM)

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
import           Network.HTTP.Types.Status    (Status (..))

import qualified Data.ByteString.Base64.Lazy  as B64
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   (ByteString)
import           Data.ByteString.Lazy.Char8   as LBS (fromStrict, length, null,
                                                      pack, unpack)
import           Data.Either.Unwrap           (fromRight)
import qualified Data.Maybe                   as DM
import           Data.Text                    as T (Text, breakOn, null,
                                                    splitOn)
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Lazy.Encoding      as TLE

import           Control.Applicative
import           Control.Exception            (SomeException, toException)
import           Control.Exception.Lifted     (throwIO)
import           Control.Monad.Base
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

import           Data.Typeable                (Typeable)
import           Data.Word                    (Word, Word8)

import           Network.HTTP.Client
import qualified Network.HTTP.Conduit         as HC

import qualified Test.Webdriver.Auth.Internal as IN



newtype WDAuth a = WDAuth {
  unWDAuth :: StateT (WDSession, Request -> Request) IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)


instance MonadBase IO WDAuth where
  liftBase = WDAuth . liftBase


instance MonadBaseControl IO WDAuth where
  type StM WDAuth a = StM (StateT (WDSession, Request -> Request) IO) a

  liftBaseWith f = WDAuth $
    liftBaseWith $ \runInBase ->
    f (\(WDAuth sT) -> runInBase $ sT)

  restoreM = WDAuth . restoreM

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
-- fromRight :: Either e a -> a
-- fromRight (Right a) = a
-- fromRight (Left _) = error "Either of type Left, not Right"

--Roughly the same as the function in Data.Either.Unwrap also called fromRight
--With added error handling for incorrect jobId format
jobIdFromRight :: Either e a -> a
jobIdFromRight (Right a) = a
jobIdFromRight (Left _) = error "Bad Job Id format"

--Queries SauceLabs for the most recent Job Id
getJobId :: String -> String -> IO (Either HttpException (Response ByteString))
getJobId user pswd = runResourceT $ do
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/msewell17/jobs?limit=1")
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
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/msewell17/jobs/" ++ (jobIdFromRight jobIdString))
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
    tReq <- liftIO $ HC.parseUrl ("https://saucelabs.com/rest/v1/msewell17/jobs/" ++ (jobIdFromRight jobIdString))
    let req = applyBasicAuth (BS.pack user) (BS.pack pswd) $ tReq {HC.method = methodPut, HC.requestHeaders = contentHeader, HC.requestBody = HC.RequestBodyBS body}
    resp <- liftIO $ try (HC.withManager ( HC.httpLbs req)) :: ResourceT IO (Either HC.HttpException (HC.Response ByteString))
    return resp
    where
      body = BS.pack "{\"passed\": false}"
      contentHeader = [(hContentType, (BS.pack "application/json"))]


--Default session configuration for pointing to Sauce Labs, the brwsr argument allows you
--to choose which browser to run the test in from the main function.
--Accepted browser arguments include chrome, firefox, and ie
sauceConfig :: Browser -> WDConfig
sauceConfig brwsr = defaultConfig { wdPort = 4444
                                  , wdHost = "ondemand.saucelabs.com"
                                  , wdCapabilities = defaultCaps {browser = brwsr}
                                  }

getApplyAuth :: WDAuth (Request -> Request)
getApplyAuth = WDAuth $ S.StateT (\v@(_, addAuth) -> return (addAuth, v))

runWDAuthWith :: Browser -> BS.ByteString -> BS.ByteString -> WDAuth a -> IO a
runWDAuthWith brwsr user pass wd = do
  sess <- mkSession $ sauceConfig brwsr
  runWDAuth user pass sess $ createSession (wdRequestHeaders $ sauceConfig brwsr) (wdCapabilities $ sauceConfig brwsr) >> wd

runWDAuth :: BS.ByteString -> BS.ByteString -> WDSession -> WDAuth a -> IO a
runWDAuth user pass sess (WDAuth wd)
  = evalStateT wd (sess, applyBasicAuth user pass)

main :: Browser -> String -> String -> IO ()
main brwsr user pswd = do
  rslt <- runWDAuthWith brwsr (BS.pack user) (BS.pack pswd) $ checkPassed user pswd loginTest
  print $ fromRight rslt

--Tests that users can log onto staging using Persona
loginTest :: WDAuth (Either IN.FailedCommand Text)
loginTest = do
  setImplicitWait 8000
  openPage "https://staging.plowtech.net/auth/login"
  persona  <- waitForPopupWindow $ findClick (ByCSS "a[href='javascript:hident2()']")
  emailBox <- checkLastCommand persona $ findFill (ByCSS "input[id='authentication_email']") "plowtech@plowtech.net"
  nextB    <- checkLastCommand emailBox $ findClick (ByCSS "button[class='isDesktop isStart isAddressInfo']")
  passBox  <- checkLastCommand nextB $ findFill (ByCSS "input[id='authentication_password']") "plowtech"
  signIn   <- waitForWindowClose $ checkLastCommand passBox (findClick (ByCSS "button[class='isReturning isTransitionToSecondary']"))
  return signIn

--Checks that the lastResult is good, sends a pass/fail to SauceLabs,
--and closes the session. Called inside the Main function.
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

--This function chains sequential commands together based on the result of the last command.
--If last command failed, it returns the Exception of the previous command.
--If last command succeeded, returns the result of running the next command and so on.
checkLastCommand :: Either IN.FailedCommand Text
                 -> WDAuth (Either IN.FailedCommand Text) -> WDAuth (Either IN.FailedCommand Text)
checkLastCommand lastResult nextCommand = do
  case lastResult of
    Left e  -> do return $ Left (e :: IN.FailedCommand)

    Right _ -> do rslt <- nextCommand
                  return rslt

--Find a form element and fill it with text
--Throws error if the element isn't found
findFill ::Selector -> Text -> WDAuth (Either IN.FailedCommand Text)
findFill selector keys = do
  form <- try $ waitUntilFound selector
  case form of
    Left e              -> do return $ Left (e :: IN.FailedCommand)

    Right (Element txt) -> do sendKeys keys (fromRight form)
                              return $ Right txt

--Find a button element and click on it
--Throws error if the element isn't found
findClick :: Selector -> WDAuth (Either IN.FailedCommand Text)
findClick selector = do
  button <- try $ waitUntilFound selector
  case button of
   Left e              -> do return $ Left (e :: IN.FailedCommand)

   Right (Element txt) -> do click $ fromRight button
                             return $ Right txt

--Similar to the same function found in /onping/tests/Selenium/WaitCommands.hs
--Also very similar to the findElem function in Test.WebDriver.Commands.Wait
waitUntilFound :: Selector -> WDAuth (Element)
waitUntilFound selector = do
  waitUntil 5000.0 $ do
    expectation <- catch (findElem selector >> return True)
                         (\(FailedCommand _ _) -> return False)
    expect expectation
  findElem selector

--Similar to the same function found in /onping/tests/Selenium/WaitCommands.hs
--Should precede a command that opens a new window.
--If the command succeeds, it focuses on the popup window.
waitForPopupWindow :: WDAuth (Either IN.FailedCommand Text) -> WDAuth (Either IN.FailedCommand Text)
waitForPopupWindow popupInitiator = do
   oldWindow <- getCurrentWindow
   rslt <- popupInitiator
   case rslt of
     Left _  -> do return rslt

     Right _ -> do waitUntil 5.0 (expect (oldWindow /= currentWindow))
                   windowList <- windows
                   let newWindow  = head $ filter (/= oldWindow) windowList
                   focusWindow newWindow
                   return rslt

--Similar to the same function found in /onping/tests/Selenium/WaitCommands.hs
--Should precede a command that closes a window.
--If the command succeeds, it focuses on the remaining window after the first closes.
waitForWindowClose :: WDAuth (Either IN.FailedCommand Text) -> WDAuth (Either IN.FailedCommand Text)
waitForWindowClose closeAction = do
   oldNumWindows <- Prelude.length `liftM`  windows
   rslt <- closeAction
   case rslt of
     Left _  -> do return rslt

     Right _ -> do waitUntil 4000.0 $ do
                     newNumWindows <- Prelude.length `liftM` windows
                     expect (oldNumWindows > newNumWindows)
                   newWindowList <- windows
                   focusWindow (head newWindowList)
                   return rslt
