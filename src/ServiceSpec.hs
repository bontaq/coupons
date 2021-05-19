module ServiceSpec where

import Test.Hspec

import Domain.Rule
import Domain.Prerequisites

import Service

spec = parallel $ do

  describe "evalRule" $ do

    let spec = Prerequisites
          { items = []
          , bundles = []
          , location = Nothing
          , codes = []
          }

    it "works for the anything-case" $ do
      evalExpression spec (Name "test")
        `shouldBe`
        Just "test"


main = hspec spec
