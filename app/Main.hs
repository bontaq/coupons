{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- for deriving the ToJSON for ErrResp
import GHC.Generics
import Control.Monad.IO.Class
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
-- Logging
import System.Log.FastLogger
-- Time
import Chronos

import Service (Cart(..), CartWithoutTime(..), runMatchCoupons)


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

addTime :: CartWithoutTime -> Time -> Cart
addTime CartWithoutTime{..} time = Cart
  { items=items
  , bundles=bundles
  , location=location
  , codes=codes
  , time=time
  }

routes :: Pool Connection -> FastLogger -> ScottyM ()
routes pool logger = do
  post "/match-discounts" $ do
    -- gets the JSON from the post, turns it into our domain object
    (cartWithoutTime :: CartWithoutTime) <- jsonData

    -- add the time
    time <- liftIO now
    let cart = addTime cartWithoutTime time

    -- what it all comes down to: do any coupons apply
    matches <- withResource pool (\conn -> runMatchCoupons conn logger cart)

    case matches of
      Left err -> json (ErrResp { message=show err })
      Right matches -> json matches


main :: IO ()
main = do
  envConfig <- getConfig

  pool <- startPool envConfig

  (logger, cleanup) <- newFastLogger (LogStdout defaultBufSize)

  scotty 3000 (routes pool logger)
