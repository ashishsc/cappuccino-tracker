{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main
  ( main
  ) where

import GHC.Generics
import System.Environment

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status (status200)
import Network.Wai.Middleware.Cors (simpleCors)
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

main :: IO ()
main = do
  connection <- connect defaultConnectInfo
  Web.Scotty.scotty 6969 $ do
    Web.Scotty.middleware simpleCors
    Web.Scotty.get "/all" $ getTotalAction connection
    Web.Scotty.post "/purchase" $ addPurchaseAction connection

getTotalAction :: Connection -> Web.Scotty.ActionM ()
getTotalAction connection = do
  liftIO $ putStrLn "fetching"
  purchaseTuples <- liftIO $ getTotalFromDb connection
    -- some time that needs to be json
  Web.Scotty.json $
    fmap
      (\(id, description, cents) -> Purchase id description cents)
      purchaseTuples

addPurchaseAction :: Connection -> Web.Scotty.ActionM ()
addPurchaseAction connection = do
  liftIO $ putStrLn "adding purchase"
  newPurchase <- Web.Scotty.jsonData :: Web.Scotty.ActionM NewPurchase
  rowsAffected <- liftIO $ addPurchaseToDb connection newPurchase
  liftIO $ putStrLn (show rowsAffected)
  Web.Scotty.status status200

getTotalFromDb :: Connection -> IO [(Int, String, Int)]
getTotalFromDb connection = do
  xs <- query_ connection "select * from cap.purchases"
  return xs

addPurchaseToDb :: Connection -> NewPurchase -> IO Int64
addPurchaseToDb connection NewPurchase {description = d, cents = c} = do
  execute
    connection
    "INSERT INTO cap.purchases (description, cents) VALUES (?, ?)"
    (d, c)
