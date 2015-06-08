{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
import           Test.WebDriver.Class
import           Test.WebDriver.Config
import           Test.WebDriver.Session

import           Data.Aeson
import           Data.Aeson.Types             (Parser, typeMismatch)
import           Network.HTTP.Client          (Request (..), RequestBody (..),
                                               Response (..), httpLbs)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status    (Status (..))

import qualified Data.ByteString.Base64.Lazy  as B64
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   (ByteString)
import           Data.ByteString.Lazy.Char8   as LBS (fromStrict, length, null,
                                                      unpack)
import           Data.Text                    as T (Text, null, splitOn)
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Lazy.Encoding      as TLE


import           Control.Applicative
import           Control.Exception            (SomeException, toException)
import           Control.Exception.Lifted     (throwIO)
import           Control.Monad.Base

import           Data.Typeable                (Typeable)
import           Data.Word                    (Word, Word8)

import           Network.HTTP.Client
import           Test.Webdriver.Auth.Internal




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
    mkRequestWith applyAuth headers method path args
    >>= sendHTTPRequest
    >>= getJSONResult
    >>= either throwIO return


getApplyAuth :: WDAuth (Request -> Request)
getApplyAuth = WDAuth $ S.StateT (\v@(_, addAuth) -> return (addAuth, v))


runWDAuthWith :: BS.ByteString -> BS.ByteString -> WDConfig -> WDAuth a -> IO a
runWDAuthWith user pass conf wd = do
  sess <- mkSession conf
  runWDAuth user pass sess wd

runWDAuth :: BS.ByteString -> BS.ByteString -> WDSession -> WDAuth a -> IO a
runWDAuth user pass sess (WDAuth wd) = evalStateT wd (sess, applyBasicAuth user pass)
