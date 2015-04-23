{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module NoteScript.Prosper
    ( prosperScript
    ) where

import           Control.Monad.Free.Church
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (void)

import           Data.Functor.Coyoneda

import           NoteScript.Syntax
import           Prosper                   as P
import           Prosper.Monad

import           Logging

interpretProsper
    :: forall a. User
    -> Listing
    -> NoteScriptF Listing InvestResponse a
    -> Prosper a
interpretProsper n l (Invest m)  = asyncProsper $ do
    x <- liftIO $ P.invest n m l
    debugM (show $ username n) (show (listingId l, investMessage x))
    return x
interpretProsper _ l (GetListing) = return l
interpretProsper _ l (Get f) = return $ f l
interpretProsper _ _ (RunIO io) = liftIO io
interpretProsper n _ (RunProsper p) = p n
interpretProsper n _ (LogS lvl s) = void . forkP $ logM (show $ username n) lvl s
interpretProsper n _ (RunLC _) = do
    warningM (show $ username n) "Wrong backend"
    error "Wrong backend"

prosperScript
    :: forall a. User
    -> Listing
    -> ProsperScript a
    -> Prosper a
prosperScript n l = iterM eval
  where eval (Coyoneda g i) = interpretProsper n l i >>= g
