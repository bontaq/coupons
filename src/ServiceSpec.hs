module ServiceSpec where

import Test.Hspec

import Domain.Rule
import Domain.Prerequisites

import Service

spec = parallel $ do

  describe "evalExpression" $ do

    let spec = Prerequisites
          { items = []
          , bundles = []
          , location = Nothing
          , codes = []
          }

    it "works for the anything-case" $ do
      evalExpression spec (Is "this coupon code")
        `shouldBe`
        Just "this coupon code"



main = hspec spec
