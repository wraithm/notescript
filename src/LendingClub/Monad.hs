{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module LendingClub.Monad where

import           Control.Concurrent        (ThreadId, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.ByteString
import           Data.Configurator
import           Data.IntSet               as S
import           Data.Vector               as V

import           LendingClub.Authorization
import           LendingClub.Commands
import           LendingClub.Listing

import           Logging

data LendingClubState = LCS
    { lcListings          :: TVar IntSet
    , lcListingState      :: TVar (Vector Listing)
    , lcProcessingQueue   :: TChan Listing
    , lcMarketDataAccount :: Authorization -- Maybe remove from TVar

    , lcApiUrl            :: !ByteString
    }

-- Maybe shouldn't have https
defaultApi :: ByteString
defaultApi = "api.lendingclub.com"

defaultAuthorization :: ByteString
defaultAuthorization = ""

defaultInvestorId :: InvestorId
defaultInvestorId = InvestorId 0

initializeLendingClub :: FilePath -> IO LendingClubState
initializeLendingClub lcConfig = do
    ls <- newTVarIO S.empty
    lss <- newTVarIO V.empty
    pq <- newBroadcastTChanIO

    config <- load [ Required lcConfig ]
    auth <- lookupDefault defaultAuthorization config "lendingclub.authorization"

    let mdAcct = Authorization auth
    au <- lookupDefault defaultApi config "lendingclub.apiurl"

    -- TODO initialize logs

    return $ LCS ls lss pq mdAcct au

type LendingClub a = ReaderT LendingClubState IO a

runLendingClub :: LendingClubState -> LendingClub a -> IO a
runLendingClub = flip runReaderT

getApiUrl :: LendingClub ByteString
getApiUrl = asks lcApiUrl

enqueueListing :: Listing -> LendingClub ()
enqueueListing l = do
    pq <- asks lcProcessingQueue
    liftIO . atomically $ writeTChan pq l

writeListingsState :: Vector Listing -> LendingClub ThreadId
writeListingsState = forkLC . updateLCVar lcListingState

getListings :: LendingClub IntSet
getListings = readLCVar lcListings

readLCVar :: (MonadIO m, MonadReader r m) => (r -> TVar a) -> m a
readLCVar x = do
    t <- asks x
    liftIO $ readTVarIO t

updateLCVar :: (MonadIO m, MonadReader r m) => (r -> TVar a) -> a -> m ()
updateLCVar x val = do
    t <- asks x
    liftIO . atomically $ writeTVar t val

mapIOLC :: (IO a -> IO b) -> LendingClub a -> LendingClub b
mapIOLC f g = ask >>= liftIO . f . runReaderT g

forkLC :: LendingClub () -> LendingClub ThreadId
forkLC = mapIOLC forkIO

asyncLC :: LendingClub a -> LendingClub (Async a)
asyncLC = mapIOLC async

waitLC :: Async a -> LendingClub a
waitLC = liftIO . wait

-- | Does not talk to LendingClub's listings endpoint.
-- This uses in-memory data to retrieve listings.
{-
listingFromNote :: Note -> LendingClub (Maybe Listing)
listingFromNote (Note { N.loanId = lid }) = do
    listings <- readLCVar lcListingState
    return $ V.find (\l -> listingId l == lid) listings
-}

updateAllListings :: LendingClub ()
updateAllListings = do
    auth <- asks lcMarketDataAccount
    listingsNow <- liftIO $ allListings auth
    addListings listingsNow
  where
    addListings :: Vector Listing -> LendingClub ()
    addListings listings = do
        _ <- writeListingsState listings

        oldListings <- getListings
        newListings <- V.foldM' (addListing oldListings) S.empty listings

        updateLCVar lcListings newListings

    addListing :: IntSet -> IntSet -> Listing -> LendingClub IntSet
    addListing oldls ls l = do
        let lid = listingId l
        when (lid `S.notMember` oldls) $ do
            infoM "LendingClub" "Detected new listing!"
            enqueueListing l
        return $ S.insert lid ls
