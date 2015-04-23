{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Prosper.Monad where

import           Control.Concurrent       (ThreadId, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.Configurator

import           Data.ByteString          (ByteString)
import           Data.IntSet              as S
import           Data.Traversable         (Traversable)
import           Data.Vector              (Vector)
import qualified Data.Vector              as V

import           Prosper.Commands
import           Prosper.Listing
import           Prosper.MarketDataUser

import           Logging

data ProsperState = PS
    { prosperListings          :: TVar IntSet
    , prosperListingState      :: TVar (Vector Listing)
    , prosperProcessingQueue   :: TChan Listing
    , prosperMarketDataAccount :: TVar MarketDataUser -- Maybe remove from TVar

    , apiUrl                   :: !ByteString -- ^ The URL for Prosper
    }
    -- TODO Add TChan of statistics updates? Write to TVar

-- TODO Might be out of date
testApi :: ByteString
testApi = "http://api.stg.circleone.com/api/"

-- | Initialize the prosper state
initializeProsper :: FilePath -> IO ProsperState
initializeProsper prosperConfig = do
    ls <- newTVarIO S.empty
    lss <- newTVarIO V.empty
    pq <- newBroadcastTChanIO

    -- Market data user
    ui <- getMDUser prosperConfig
    mdAcct <- newTVarIO ui

    -- Api URL
    config <- load [ Required prosperConfig ]
    au <- lookupDefault testApi config "prosper.apiurl"

    -- Initialize MarketData user log
    initializeMDLog ui

    return $ PS ls lss pq mdAcct au

-- | The main Monad for the Prosper backend
type Prosper a = ReaderT ProsperState IO a

-- | Alias to run Prosper monad actions
runProsper :: ProsperState -> Prosper a -> IO a
runProsper = flip runReaderT

-- | Get userinfo in the Prosper monad
getApiUrl :: Prosper ByteString
getApiUrl = asks apiUrl

-- TODO Document these, also, these could be used elsewhere
updateProsperVar :: (MonadIO m, MonadReader r m) => (r -> TVar a) -> a -> m ()
updateProsperVar x val = do
    t <- asks x
    liftIO . atomically $ writeTVar t val

modifyProsperVar :: (MonadIO m, MonadReader r m) => (r -> TVar a) -> (a -> a) -> m ()
modifyProsperVar x f = do
    t <- asks x
    liftIO . atomically $ modifyTVar' t f

readProsperVar :: (MonadIO m, MonadReader r m) => (r -> TVar a) -> m a
readProsperVar x = do
    t <- asks x
    liftIO $ readTVarIO t

getListings :: Prosper IntSet
getListings = readProsperVar prosperListings

writeListingsState :: Vector Listing -> Prosper ThreadId
writeListingsState = forkP . updateProsperVar prosperListingState

getAllListings :: Prosper (Vector Listing)
getAllListings = readProsperVar prosperListingState

getMarketDataUser :: Prosper MarketDataUser
getMarketDataUser = readProsperVar prosperMarketDataAccount

enqueueListing :: Listing -> Prosper ()
enqueueListing l = do
    pq <- asks prosperProcessingQueue
    liftIO . atomically $ writeTChan pq l

prosperConcurrently :: Traversable t => (a -> Prosper b) -> t a -> Prosper (t b)
prosperConcurrently f t = do
    r <- ask
    liftIO $ mapConcurrently (\n -> runReaderT (f n) r) t

mapIOP :: (IO a -> IO b) -> Prosper a -> Prosper b
mapIOP f g = ask >>= liftIO . f . runReaderT g

forkP :: Prosper () -> Prosper ThreadId
forkP = mapIOP forkIO

mapForkP :: (a -> Prosper ()) -> [a] -> Prosper [ThreadId]
mapForkP f = mapM (forkP . f)

asyncProsper :: Prosper a -> Prosper (Async a)
asyncProsper = mapIOP async

waitProsper :: Async a -> Prosper a
waitProsper = liftIO . wait

-- | Poll for listings via the JSON endpoint.
-- Check for new listings. 'enqueue' new listings onto the
-- processing queue 'TChan'.
updateAllListings :: Prosper ()
updateAllListings = do
    listingsNow <- liftIO . allListings . mdUser =<< getMarketDataUser
    addListings listingsNow
  where
    addListings :: Vector Listing -> Prosper ()
    addListings listings = do
        _ <- writeListingsState listings

        oldListings <- getListings
        newListings <- V.foldM' (addListing oldListings) S.empty listings

        updateProsperVar prosperListings newListings

    addListing :: IntSet -> IntSet -> Listing -> Prosper IntSet
    addListing oldls ls l = do
        let lid = listingId l
        when (lid `S.notMember` oldls) $ do
            MDUser { logger = lg } <- getMarketDataUser
            infoM lg "Detected new listing!"
            enqueueListing l
        return $ S.insert lid ls
