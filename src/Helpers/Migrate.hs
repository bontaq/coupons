-- |

module Helpers.Migrate where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

import Data.ByteString.Char8

type ConnectionString = ByteString

migrate :: IO (MigrationResult String)
migrate = do
  let url = "host=localhost dbname=coupon user=coupon password=password"
  let dir = "./migrations"
  con <- connectPostgreSQL (pack url)
  withTransaction con $ do
    runMigration $ MigrationContext MigrationInitialization True con
    runMigration $ MigrationContext (MigrationDirectory dir) True con

migrateForTests :: ConnectionString -> IO ()
migrateForTests connectionString = do
  let dir = "./migrations"
  con <- connectPostgreSQL connectionString
  withTransaction con $ do
    runMigration $ MigrationContext MigrationInitialization True con
    runMigration $ MigrationContext (MigrationDirectory dir) True con

  pure ()
