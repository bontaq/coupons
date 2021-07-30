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
import Web.Scotty hiding (header)
-- Env parsing
import Env

import Service


-------------------
-- Database pool --
-------------------

defaultHost     = "localhost"
defaultUser     = "coupon"
defaultPassword = "password"
defaultDatabase = "coupon"

startPool :: Config -> IO (Pool Connection)
startPool config =
  let connectionInfo = defaultConnectInfo
        { connectHost = databaseHost config
        , connectUser = databaseUser config
        , connectPassword = databasePassword config
        , connectDatabase = databaseName config
        }
  in
    createPool (connect connectionInfo) close 1 10 10


---------
-- Env --
---------

data Config = Config
  { databaseHost :: String
  , databasePassword :: String
  , databaseUser :: String
  , databaseName :: String
  }

getConfig :: IO Config
getConfig = Env.parse (header "envparse example") $
  Config
    <$> var str "DATABASE_HOST"
      (def defaultHost <> help "Host location for the database")
    <*> var str "DATABASE_PASSWORD"
      (def defaultPassword <> help "Password for the database")
    <*> var str "DATABASE_USER"
      (def defaultUser <> help "User for the database")
    <*> var str "DATABASE_NAME"
      (def defaultDatabase <> help "The name of the database")


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
  envConfig <- getConfig

  pool <- startPool envConfig

  scotty 3000 (routes pool)
