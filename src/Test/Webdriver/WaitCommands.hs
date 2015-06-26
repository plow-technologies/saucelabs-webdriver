module Test.Webdriver.WaitCommands where

import           Control.Monad
import           Control.Monad.Catch
import           Data.Text                    hiding (filter, head, length,
                                               null)
import           Test.WebDriver
import           Test.Webdriver.Auth
import qualified Test.Webdriver.Auth.Internal as IN
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait


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
