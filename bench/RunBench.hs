module Main where

import           Criterion.Main

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)

import           Data.Vector            (Vector)
import qualified Data.Vector            as V

import           NoteScript
import           Prosper                hiding (Money)
import           Prosper.MarketDataUser
import           Prosper.Monad

withMDUser :: (User -> Prosper a) -> Prosper a
withMDUser f = mdUser <$> getMarketDataUser >>= f

principalOnActive :: Vector Note -> Money
principalOnActive = V.sum . V.map principalBalance

vectorBench :: Prosper Money
vectorBench = withMDUser $ \n -> liftIO $ do
    ns <- notes n
    return (principalOnActive ns)

main :: IO ()
main = do
    ps <- initializeProsper "prosper.cfg"
    let runBench = nfIO . runProsper ps
    defaultMain
        [ bgroup "Principal on Active Notes"
            [ bench "Vectors" (runBench vectorBench)
            ]
        ]
