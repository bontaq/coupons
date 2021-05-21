{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- for deriving the ToJSON for ErrResp
import GHC.Generics
-- DB
import Database.PostgreSQL.Simple
-- JSON handling
import Data.Aeson hiding (json)
-- Server
import Web.Scotty

import Service

newtype ErrResp = ErrResp
  { message :: String
  } deriving (Generic)

instance ToJSON ErrResp where

routes :: Connection -> ScottyM ()
routes conn = do
  post "/discounts" $ do
    -- gets the JSON from the post, turns it into our domain object
    (context :: Context) <- jsonData

    -- what it all comes down to: do any coupons apply?
    actions <- runGetActions conn context

    case actions of
      Left err -> json (ErrResp { message=show err })
      Right actions -> json actions

main :: IO ()
main = do
  -- here we could access the Env / read settings from a file / build a nice command line tool
  conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"

  -- it expects a database connection, we should be passing in a pool instead of using the
  -- same connection
  scotty 3000 (routes conn)
