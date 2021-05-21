{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- for deriving the ToJSON for ErrResp
import GHC.Generics
-- DB
import Database.PostgreSQL.Simple
-- DB Pool
import Data.Pool
-- JSON handling
import Data.Aeson hiding (json)
-- Server
import Web.Scotty

import Service


-------------------
-- Database pool --
-------------------

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo {
      connectHost = "localhost"
    , connectUser = "coupon"
    , connectPassword = "password"
    , connectDatabase = "coupon"
  }

startPool :: IO (Pool Connection)
startPool = createPool (connect connectionInfo) close 1 10 10


------------
-- Server --
------------

newtype ErrResp = ErrResp { message :: String } deriving (Generic)

instance ToJSON ErrResp where

routes :: Pool Connection -> ScottyM ()
routes pool = do
  post "/discounts" $ do
    -- gets the JSON from the post, turns it into our domain object
    (context :: Context) <- jsonData

    -- what it all comes down to: do any coupons apply?
    actions <- withResource pool (\conn -> runGetActions conn context)

    case actions of
      Left err -> json (ErrResp { message=show err })
      Right actions -> json actions

main :: IO ()
main = do
  -- here we could access the Env / read settings from a file / build a nice command line tool
  pool <- startPool
  -- conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"

  -- it expects a database connection, we should be passing in a pool instead of using the
  -- same connection
  scotty 3000 (routes pool)
