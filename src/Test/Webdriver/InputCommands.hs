module Test.Webdriver.InputCommands where

import           Control.Monad.Catch
import           Data.Either.Unwrap           (fromRight)
import           Data.Text
import           Test.WebDriver
import           Test.Webdriver.Auth
import qualified Test.Webdriver.Auth.Internal as IN
import           Test.Webdriver.WaitCommands

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
