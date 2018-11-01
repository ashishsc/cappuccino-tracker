{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main
  ( main
  ) where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Database.PostgreSQL.Simple
import GHC.Generics
import Network.HTTP.Types.Status (status200, status201)
import Network.Wai.Middleware.Cors (simpleCors)
import System.Environment (getEnvironment, lookupEnv)
import qualified Web.Scotty

data Purchase = Purchase
  { id :: Int
  , description :: String
  , cents :: Int
  } deriving (Generic, ToJSON, FromJSON)

data NewPurchase = NewPurchase
  { description :: String
  , cents :: Int
  } deriving (Generic, ToJSON, FromJSON)

data NewCapPurchase = NewCapPurchase
  { shop :: String
  , cents :: Int
  } deriving (Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  maybeEnvDbUrl <- lookupEnv "DB_URL"
  let defaultConnString = postgreSQLConnectionString defaultConnectInfo
  connection <-
    connectPostgreSQL $ fromMaybe defaultConnString (fmap pack maybeEnvDbUrl)
  Web.Scotty.scotty 6969 $ do
    Web.Scotty.middleware simpleCors
    Web.Scotty.get "/all" $ getTotalAction connection
    Web.Scotty.get "/cap-total" $ getCapSumAction connection
    Web.Scotty.post "/purchase" $ addPurchaseAction connection
    Web.Scotty.post "/buy-cap" $ buyCapAction connection

getTotalAction :: Connection -> Web.Scotty.ActionM ()
getTotalAction connection = do
  liftIO $ putStrLn "fetching"
  purchaseTuples <- liftIO $ getTotalFromDb connection
    -- some time that needs to be json
  Web.Scotty.json $
    fmap
      (\(id, description, cents) -> Purchase id description cents)
      purchaseTuples

getCapSumAction :: Connection -> Web.Scotty.ActionM ()
getCapSumAction connection = do
  liftIO $ putStrLn "fetch aggregate cappucino purchases"
  capSum <- liftIO $ getCapSumFromDb connection
  Web.Scotty.json $ capSum

addPurchaseAction :: Connection -> Web.Scotty.ActionM ()
addPurchaseAction connection = do
  liftIO $ putStrLn "adding purchase"
  newPurchase <- Web.Scotty.jsonData :: Web.Scotty.ActionM NewPurchase
  rowsAffected <- liftIO $ addPurchaseToDb connection newPurchase
  liftIO $ putStrLn (show rowsAffected)
  Web.Scotty.status status201

buyCapAction :: Connection -> Web.Scotty.ActionM ()
buyCapAction connection = do
  liftIO $ putStrLn "buying cappuccino"
  newCap <- Web.Scotty.jsonData :: Web.Scotty.ActionM NewCapPurchase
  rowsAffected <- liftIO $ addCapPurchaseToDb connection newCap
  liftIO $ putStrLn (show rowsAffected)
  Web.Scotty.status status201

getTotalFromDb :: Connection -> IO [(Int, String, Int)]
getTotalFromDb connection = do
  xs <- query_ connection "select * from cap.purchases"
  return xs

getCapSumFromDb :: Connection -> IO Int
getCapSumFromDb connection = do
  capSum <- query_ connection "select sum(cents) from cap.caps"
  return $ fromOnly $ head capSum

addPurchaseToDb :: Connection -> NewPurchase -> IO Int64
addPurchaseToDb connection NewPurchase {description = d, cents = c} = do
  execute
    connection
    "INSERT INTO cap.purchases (description, cents) VALUES (?, ?)"
    (d, c)

addCapPurchaseToDb :: Connection -> NewCapPurchase -> IO Int64
addCapPurchaseToDb connection NewCapPurchase {shop = s, cents = c} = do
  execute connection "INSERT INTO cap.caps (shop, cents) VALUES (?, ?)" (s, c)
