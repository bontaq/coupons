module Domain.RuleRepositorySpec where

import Control.Carrier.Lift
import Control.Carrier.State.Strict

import Test.Hspec

import Domain.RuleRepository
import Domain.Rule


spec = do
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


main = hspec spec
