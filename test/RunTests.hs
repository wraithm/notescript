module Main where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Runners.AntXML -- For cabal builds on Jenkins

import Control.Applicative
import Control.Monad.IO.Class

import Prosper
import Prosper.Monad
import Prosper.MarketDataUser

noteTestGroup :: ProsperState -> TestTree
noteTestGroup ps = testGroup "Notes" 
    [ testCase "parses at all" (runProsper ps parsesNoteAtAll) 
    ]

withMDUser :: (User -> Prosper a) -> Prosper a
withMDUser f = mdUser <$> getMarketDataUser >>= f

parsesNoteAtAll :: Prosper ()
parsesNoteAtAll = withMDUser $ \n -> liftIO $
   notes n >>= print

main :: IO ()
main = do
    ps <- initializeProsper "prosper.cfg"
    defaultMain $ testGroup "Tests"
        [ noteTestGroup ps
        ]
