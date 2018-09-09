{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment

import Web.Scotty
import Database.PostgreSQL.Simple


import Lib

main :: IO ()
main = someFunc
