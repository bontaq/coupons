{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.RuleRepositorySpec where

import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Carrier.Reader

import GHC.IO
import Database.PostgreSQL.Simple hiding (In)
import Database.Postgres.Temp

import Chronos

import Test.Hspec

import Helpers.Migrate
import Domain.RuleRepository
import Domain.Rule
import Domain.Shared


spec = parallel $ do

  describe "getCodes" $ do

    it "finds code in HasCode" $ do
      getCodes (Has (Code "test") $ Is "")
        `shouldBe`
        ["test"]

    it "finds the codes in OneOf" $ do
      getCodes (OneOf [Has (Code "test") $ Is "", Has (Code "2") $ Is ""] $ Is "")
        `shouldBe`
        ["test", "2"]

    it "finds a code in DateRange" $ do
      let time = datetimeToTime $ Datetime (Date (Year 1989) (Month 10) (DayOfMonth 18)) (TimeOfDay 0 0 0)
      getCodes (Between time time (Has (Code "test") $ Is ""))
        `shouldBe`
        ["test"]

    it "finds a code in HasItem" $ do
      getCodes (Has (One "bike") (Has (Code "test") $ Is ""))
        `shouldBe`
        ["test"]

    it "finds a code in Locale" $ do
      getCodes (In [Country "US"] (Has (Code "test") $ Is ""))
        `shouldBe`
        ["test"]

    it "doesn't find a code in Name" $ do
      getCodes (Is "")
        `shouldBe`
        []

    it "finds a nested code" $ do
      getCodes (In [Country "US"] $ Has (One "bike") $ Has (Code "test") $ Is "")
        `shouldBe`
        ["test"]

  describe "RuleRepoState" $ do

    let emptyState = RuleState { rules=[], closedRules=[] }
        repo = run . execState emptyState . runRuleRepo
        repoWithState state = run . execState state . runRuleRepo

    describe "addRule" $ do

      it "Stores new rules" $ do
        let newRule = Rule (Is "test") []

        repo (addRule newRule)
          `shouldBe`
          RuleState { rules=[newRule], closedRules=[] }

      it "Stores a new closed rule" $ do
        let closedRule = Rule (Has (Code "test-99") (Is "test")) []

        repo (addRule closedRule)
          `shouldBe`
          RuleState { rules=[], closedRules=[(closedRule, "test-99")] }

    describe "getClosedRule" $ do

      it "gets a rule by code" $ do
        let closedRule = Rule (Has (Code "test-99") (Is "test")) []
            -- evalState is so we get the result instead of the
            -- new state at the end of the do block
            repo = runM . evalState emptyState . runRuleRepo

        r <- repo $ do
          addRule closedRule
          getClosedRules ["test-99"]

        r `shouldBe` Right [closedRule]

      it "returns a DNE for no matches" $ do
        let repo = run . evalState emptyState . runRuleRepo

        repo (getClosedRules ["test-99"])
          `shouldBe`
          Right []

    describe "getOpenRules" $ do

      it "Returns a list of all rules (empty state)" $ do
        repo getOpenRules
          `shouldBe`
          RuleState { rules=[], closedRules=[] }

      it "Returns a list of all rules (state with a rule in it)" $ do
        let stateWithRule = emptyState { rules=[Rule (Is "test") []] }

        repoWithState stateWithRule getOpenRules
          `shouldBe`
          RuleState { rules=[Rule (Is "test") []], closedRules=[] }


  describe "RuleRepoIO" $ do
    --
    -- For testing we're creating a temporary DB, migrating it,
    -- and then running each test inside a transaction. This could
    -- all be moved into Helpers
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

      -- the magic that runs the open action before the test, and runs close after
      withDBConnection =
        bracket (openConnection $ toConnectionString tempDB) closeConnection

      destroyTempDB _ = do
        stop tempDB
        putStrLn "\nTemp DB removed\n"

    --
    -- The actual tests
    --
    -- afterAll runs after the test block is done, removing the temp db
    --
    -- around passes into each test the connection to the db,
    -- opening it before and closing it after the test has run. It also
    -- handles the opening & rollback of the transaction.
    --
    afterAll destroyTempDB $ around withDBConnection $ do

      let mkRepo dbConn = runM . runReader dbConn . runRuleRepoIO

      describe "addRule" $ do

        it "Stores new rules" $ \connection -> do
          let
            newRule = Rule (Is "test") []
            repo = mkRepo connection

          repo (addRule newRule)

          rules <- repo getOpenRules
          rules
            `shouldBe`
            Right [newRule]

        it "Stores a rule with a code as a closed rule" $ \connection -> do
          let
            ruleWithCode = Rule (Has (Code "test-99") (Is "test-99-coupon")) []
            repo = mkRepo connection

          repo (addRule ruleWithCode)


          openRules <- repo getOpenRules
          openRules `shouldBe` Right []
          closedRule <- repo (getClosedRules ["test-99"])
          closedRule `shouldBe` Right [ruleWithCode]

        it "Stores a rule with multiple codes as separate closed rules" $ \connection -> do
          let
            ruleWithCodes =
              Rule (OneOf
                    [ Has (Code "50off") (Is "50off")
                    , Has (Code "50free") (Is "50free")
                    ]
                   (Is "test"))
                   []
            repo = mkRepo connection

          repo (addRule ruleWithCodes)

          fiftyOffRule <- repo (getClosedRules ["50off"])
          fiftyOffRule `shouldBe` Right [ruleWithCodes]
          fiftyFreeRule <- repo (getClosedRules ["50free"])
          fiftyFreeRule `shouldBe` Right [ruleWithCodes]

      describe "getClosedRule" $ do

        it "returns a rule for a code" $ \connection -> do
          let
            closedRule = Rule (Has (Code "test") (Is "test")) []
            repo = mkRepo connection

          repo (addRule closedRule)
          rule <- repo (getClosedRules ["test"])
          rule `shouldBe` Right [closedRule]

        it "returns an error for a code that doesn't exist" $ \connection -> do
          let repo = mkRepo connection

          rule <- repo (getClosedRules ["DNE"])
          rule `shouldBe` Right []

      describe "getOpenRules" $ do

        it "Returns a list of all rules (empty state)" $ \connection -> do
          let repo = mkRepo connection

          rules <- repo getOpenRules
          rules `shouldBe` Right []

        it "Returns a list of all rules (after adding a rule)" $ \connection -> do
          -- hey wait this is pretty much the same tests as above.  shrug shrug
          let
            newRule = Rule (Is "test") []
            repo = mkRepo connection

          repo (addRule newRule)
          repo (addRule newRule)

          rules <- repo getOpenRules
          rules
            `shouldBe`
            Right [newRule, newRule]

-- for running from the repl
main = hspec spec
