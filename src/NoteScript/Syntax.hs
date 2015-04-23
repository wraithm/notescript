{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module NoteScript.Syntax
    ( NoteScriptF(..)
    , NoteScript
    , ProsperScript
    , LendingClubScript

    , invest
    , getListing
    , get
    , runIO
    , execProsper
    , execLC
    , logS
    , debug
    ) where

import           Control.Concurrent.Async  (Async)
import           Control.Monad.Free.Church (F, liftF)

import           Data.Functor.Coyoneda     (Coyoneda, liftCoyoneda)

import qualified LendingClub               as LC
import           LendingClub.Monad
import qualified Prosper                   as P
import           Prosper.Monad

import           Logging

type Money = Double

data NoteScriptF l ir a where
    Invest      :: Money -> NoteScriptF l ir (Async ir)
    GetListing  :: NoteScriptF l ir l
    Get         :: (l -> b) -> NoteScriptF l ir b
    RunIO       :: IO b -> NoteScriptF l ir b
    RunProsper  :: (P.User -> Prosper b) -> NoteScriptF l ir b
    RunLC       :: (LC.Authorization -> LC.InvestorId -> LendingClub b) -> NoteScriptF l ir b
    LogS        :: Priority -> String -> NoteScriptF l ir ()

type NoteScript l ir = F (Coyoneda (NoteScriptF l ir))
type ProsperScript = NoteScript P.Listing P.InvestResponse
type LendingClubScript = NoteScript LC.Listing LC.InvestResponse

singleton :: forall l ir a. NoteScriptF l ir a -> NoteScript l ir a
singleton = liftF . liftCoyoneda

invest :: Money -> NoteScript l ir (Async ir)
invest = singleton . Invest

getListing :: NoteScript l ir l
getListing = singleton GetListing

get :: (l -> a) -> NoteScript l ir a
get = singleton . Get

runIO :: IO a -> NoteScript l ir a
runIO = singleton . RunIO

execProsper :: (P.User -> Prosper a) -> NoteScript P.Listing ir a
execProsper = singleton . RunProsper

execLC :: (LC.Authorization -> LC.InvestorId -> LendingClub a) -> NoteScript LC.Listing ir a
execLC = singleton . RunLC

logS :: Priority -> String -> NoteScript l ir ()
logS lvl = singleton . LogS lvl

debug :: Show a => a -> NoteScript l ir ()
debug = logS DEBUG . show
