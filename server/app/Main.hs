{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment

import Web.Scotty
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import Control.Monad.Trans (liftIO)
import Database.PostgreSQL.Simple




main :: IO ()
main = do
  connection <- connect defaultConnectInfo
  scotty 6969 $ do
    get "/all" $ getTotalAction connection



getTotalAction :: Connection -> ActionM ()
getTotalAction connection = do
    liftIO $ putStrLn "fetching"
    purchaseTuples <- liftIO $ fmap (LazyText.pack . show) $ getTotalFromDb connection

    -- some time that needs to be json
    json purchaseTuples

getTotalFromDb :: Connection -> IO [(Int, String, Int)]
getTotalFromDb connection = do
  xs <- query_ connection "select * from cap.purchases"
  return xs