module Test.Webdriver.InputCommands where

import           Data.Text
import           Test.Webdriver.Auth
import qualified Test.Webdriver.Auth.Internal as IN

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
