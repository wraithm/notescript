{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module NoteScript.LendingClub
    ( lendingClubScript
    ) where

import           Control.Monad.Free.Church
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (void)

import           Data.Functor.Coyoneda

import           LendingClub               as LC
import           LendingClub.Monad
import           Logging                   (debugM, logM, warningM)
import           NoteScript.Syntax

interpretLendingClub
    :: forall a. Authorization
    -> InvestorId
    -> Listing
    -> NoteScriptF Listing InvestResponse a
    -> LendingClub a
interpretLendingClub n i l (Invest m)  = asyncLC $ do
    x <- liftIO $ LC.invest n i m l
    debugM (show i) (show (i, listingId l, investMessage x))
    return x
  where
    investMessage (InvestResponse { orderConfirmations = (InvestConfirmation {executionStatus = status }):_}) =
        show status
    investMessage _ = show "No executionStatus"
interpretLendingClub _ _ l (GetListing) = return l
interpretLendingClub _ _ l (Get f) = return $ f l
interpretLendingClub _ _ _ (RunIO io) = liftIO io
interpretLendingClub n i _ (RunLC lc) = lc n i
interpretLendingClub _ i _ (LogS lvl s) = void . forkLC $ logM (show i) lvl s
interpretLendingClub _ i _ (RunProsper _) = do
    warningM (show i) "Wrong backend"
    error "Wrong backend"

lendingClubScript
    :: forall a. Authorization
    -> InvestorId
    -> Listing
    -> LendingClubScript a
    -> LendingClub a
lendingClubScript n invid l = iterM eval
  where eval (Coyoneda g i) = interpretLendingClub n invid l i >>= g
