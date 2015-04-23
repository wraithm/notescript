{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Prosper.MarketDataUser
    ( MarketDataUser(..)
    , getMDUser
    , initializeMDLog
    ) where

import           Control.Monad             (when)

import           Data.ByteString           (ByteString)
import           Data.Configurator
import           Data.Configurator.Types   (Value (..))

import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (setHandlers, setLevel,
                                            updateGlobalLogger)

import           Logging
import           Prosper.User

data MarketDataUser = MDUser
    { mdUser   :: !User
    , logger   :: !String -- ^ Name of the logger for this user, defaults to username value
    , enabled  :: !Bool -- ^ Is the logger enabled?
    , location :: !FilePath -- ^ Path to log file for user
    , level    :: !Priority -- ^ Logging level
    } deriving (Show, Eq)

convertPriority :: Value -> Priority
convertPriority (String "DEBUG") = DEBUG
convertPriority (String "INFO") = INFO
convertPriority (String "NOTICE") = NOTICE
convertPriority (String "WARNING") = WARNING
convertPriority (String "ERROR") = ERROR
convertPriority (String "CRITICAL") = CRITICAL
convertPriority (String "ALERT") = ALERT
convertPriority (String "EMERGENCY") = EMERGENCY
convertPriority _ = INFO

-- Default test info
testUsername :: ByteString
testUsername = ""

testPassword :: ByteString
testPassword = ""

-- | Get the user info for authorizing GETs and POSTs to Prosper
getMDUser :: FilePath -> IO MarketDataUser
getMDUser prosperConfig = do
    config <- load [ Required prosperConfig ]
    un <- lookupDefault testUsername config "prosper.username" -- TODO move these to other variables
    pwd <- lookupDefault testPassword  config "prosper.password"

    l <- lookupDefault (show un) config "prosper.log.logger"
    e <- lookupDefault True config "prosper.log.enabled"
    loc <- lookupDefault "prosper.log" config "prosper.log.location"
    lvl <- lookupDefault (String "INFO") config "prosper.log.level"

    return $ MDUser (User un pwd) l e loc (convertPriority lvl)

-- | Initialize the logger for this user
initializeMDLog :: MarketDataUser -> IO ()
initializeMDLog MDUser{..} = when enabled $ do
    fh <- withFormatter `fmap` fileHandler location level

    updateGlobalLogger logger (setLevel level)
    updateGlobalLogger logger (setHandlers [fh])
