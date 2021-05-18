{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.RuleRepositorySpec where

import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Carrier.Reader

import GHC.IO
import Database.PostgreSQL.Simple
import Database.Postgres.Temp

import Test.Hspec

import Helpers.Migrate
import Domain.RuleRepository
import Domain.Rule


spec = parallel $ do
  describe "RuleRepoState" $ do

    let emptyState = RuleState { rules=[] }
        repo = run . execState emptyState . runRuleRepo
        repoWithState state = run . execState state . runRuleRepo

    describe "addRule" $ do

      it "Stores new rules" $ do
        let newRule = Rule (Name "test") []

        repo (addRule newRule)
          `shouldBe`
          RuleState { rules=[newRule] }

    describe "getRules" $ do

      it "Returns a list of all rules (empty state)" $ do
        repo getRules
          `shouldBe`
          RuleState { rules=[] }

      it "Returns a list of all rules (state with a rule in it)" $ do
        let stateWithRule = emptyState { rules=[Rule (Name "test") []] }

        repoWithState stateWithRule getRules
          `shouldBe`
          RuleState { rules=[Rule (Name "test") []] }


  describe "RuleRepoIO" $ do
    --
    -- For testing we're creating a temporary DB, migrating it,
    -- and then running each test inside a transaction.
    --

    let dbOrCrash value = case value of
          Right value' -> value'
          -- crash and burn if we can't start a temp db
          Left errStr  -> error ("Could not acquire temporary db: " <> show errStr)

    -- get a new temp db
    tempDB <- dbOrCrash <$> runIO start
    -- run migrations
    runIO $ migrateForTests (toConnectionString tempDB)

    let
      -- Enter cool zone
      openConnection connectionString = do
        conn <- connectPostgreSQL connectionString
        begin conn -- we wrap each test in a transaction
        pure conn

      closeConnection conn = do
        rollback conn -- and then roll back the transaction when the test is over
        close conn

      withDBConnection =
        bracket (openConnection $ toConnectionString tempDB) closeConnection

      destroyTempDB _ = do
        stop tempDB
        putStrLn "\nTemp DB removed"

    afterAll destroyTempDB $ around withDBConnection $ do

      let mkRepo dbConn = runM . runReader dbConn . runRuleRepoIO

      describe "addRule" $ do

        it "Stores new rules" $ \connection -> do
          let
            newRule = Rule (Name "test") []
            repo = mkRepo connection

          repo (addRule newRule)

          rules <- repo getRules
          rules
            `shouldBe`
            Right [newRule]

      describe "getRules" $ do

        it "Returns a list of all rules (empty state)" $ \connection -> do
          let repo = mkRepo connection

          rules <- repo getRules
          rules `shouldBe` Right []

        it "Returns a list of all rules (after adding a rule)" $ \connection -> do
          -- hey wait this is pretty much the same tests as above.  shrug shrug
          let
            newRule = Rule (Name "test") []
            repo = mkRepo connection

          repo (addRule newRule)
          repo (addRule newRule)

          rules <- repo getRules
          rules
            `shouldBe`
            Right [newRule, newRule]


main = hspec spec
