{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import System.Environment
import GHC.Generics

import qualified Web.Scotty
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import Control.Monad.Trans (liftIO)
import Database.PostgreSQL.Simple
import Data.Aeson

data Purchase = Purchase { id :: Int
                         , description :: String
                         , cents :: Int
                         } deriving (Generic, ToJSON, FromJSON)



main :: IO ()
main = do
  connection <- connect defaultConnectInfo
  Web.Scotty.scotty 6969 $ do
    Web.Scotty.get "/all" $ getTotalAction connection



getTotalAction :: Connection -> Web.Scotty.ActionM ()
getTotalAction connection = do
    liftIO $ putStrLn "fetching"
    purchaseTuples <- liftIO $ getTotalFromDb connection

    -- some time that needs to be json
    Web.Scotty.json $ fmap (\(id, description, cents) -> Purchase {Main.id = id, description = description, cents = cents}) purchaseTuples

getTotalFromDb :: Connection -> IO [(Int, String, Int)]
getTotalFromDb connection = do
  xs <- query_ connection "select * from cap.purchases"
  return xs