module Logging
    ( initializeLogs
    , withFormatter

    -- From System.Log.Logger
    , logM, infoM, debugM, warningM, errorM, alertM, noticeM, Priority (..)
    ) where

import           Control.Monad.IO.Class    (MonadIO (..), liftIO)

import           System.IO                 (Handle, stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (GenericHandler, streamHandler)
import           System.Log.Logger         (Priority (..), rootLoggerName,
                                            setHandlers, setLevel)
import qualified System.Log.Logger         as L

initializeLogs :: IO ()
initializeLogs = do
    fh <- withFormatter `fmap` streamHandler stdout DEBUG

    liftIO $ L.updateGlobalLogger rootLoggerName (setHandlers [fh])
    liftIO $ L.updateGlobalLogger rootLoggerName (setLevel DEBUG)

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter h = setFormatter h (simpleLogFormatter format)

format :: String
format = "[$time $loggername $prio] $msg"

-- Convert all hslogger functions to use liftIO
alertM :: MonadIO m
      => String                   -- ^ Logger name
      -> String                   -- ^ Log message
      -> m ()
alertM n m = liftIO $ L.alertM n m

debugM :: MonadIO m
      => String                   -- ^ Logger name
      -> String                   -- ^ Log message
      -> m ()
debugM n m = liftIO $ L.debugM n m

errorM :: MonadIO m
      => String                   -- ^ Logger name
      -> String                   -- ^ Log message
      -> m ()
errorM n m = liftIO $ L.errorM n m

infoM :: MonadIO m
      => String                   -- ^ Logger name
      -> String                   -- ^ Log message
      -> m ()
infoM n m = liftIO $ L.infoM n m

warningM :: MonadIO m
      => String                   -- ^ Logger name
      -> String                   -- ^ Log message
      -> m ()
warningM n m = liftIO $ L.warningM n m

noticeM :: MonadIO m
      => String                   -- ^ Logger name
      -> String                   -- ^ Log message
      -> m ()
noticeM n m = liftIO $ L.noticeM n m

logM :: MonadIO m
     => String                     -- ^ Name of the logger to use
     -> Priority                   -- ^ Priority of this message
     -> String                     -- ^ The log text itself
     -> m ()
logM n p m = liftIO $ L.logM n p m
