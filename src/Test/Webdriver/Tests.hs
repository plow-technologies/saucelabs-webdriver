{-# LANGUAGE OverloadedStrings #-}

module Test.Webdriver.Tests where

import qualified Data.ByteString.Char8        as BS
import           Data.Either.Unwrap           (fromRight)
import           Data.Text
import           Test.WebDriver
import           Test.Webdriver.Auth
import qualified Test.Webdriver.Auth.Internal as IN
import           Test.Webdriver.InputCommands
import           Test.Webdriver.WaitCommands

main :: Browser -> Int -> String -> String -> IO ()
main brwsr vers user pswd = do
  rslt <- runWDAuthWith brwsr vers (BS.pack user) (BS.pack pswd) $ checkPassed user pswd loginTest
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
