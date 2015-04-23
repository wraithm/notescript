{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prosper.JSON
import Prosper
import Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    ps <- initializeProsper "prosper.cfg"
    runProsper ps $ do
        md <- getMarketDataUserInfo
        let ui = mdUserInfo md

        listings <- csvGet ui "Listings"

        liftIO $ B.putStrLn listings
