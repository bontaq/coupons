{-# LANGUAGE DuplicateRecordFields #-}
module ServiceSpec where

import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Effects.Logging (runLogIO)

import Chronos
import Torsor

import Test.Hspec

import Domain.Rule
import Domain.RuleRepository
import Domain.Cart


import Service

spec = parallel $ do

  let cart = Cart
        { items = []
        , bundles = []
        , location = Nothing
        , codes = []
        , time = datetimeToTime (Datetime (Date (Year 1989) (Month 10) (DayOfMonth 0)) (TimeOfDay 0 0 0))
        }

  describe "findAllSlugs" $ do

    it "works fine with empty information" $ do
      findAllSlugs cart `shouldBe` []

    it "finds the slugs in items" $ do
      findAllSlugs (cart{ items=[Item { slug="cat" }] })
        `shouldBe` ["cat"]

    it "find the bundle's name slug" $ do
      findAllSlugs (cart{ bundles=[Bundle { items=[], slug="a package" }] })
        `shouldBe` ["a package"]

    it "finds the slugs in bundles" $ do
      let item = Item { slug="cat" }

      findAllSlugs (cart{ bundles=[Bundle { items=[item], slug="bundle" }] })
        `shouldBe` ["cat", "bundle"]

    it "works with bundles and items" $ do
      let item = Item { slug="cat-item" }
      let bundle = Bundle { items=[item { slug="bundle-item" }], slug="a-bundle" }

      findAllSlugs cart{ bundles=[bundle], items=[item] }
        `shouldBe` ["cat-item", "bundle-item", "a-bundle"]

  describe "evalExpression" $ do

    -- TODO: Could I use property testing for this?  Not sure

    it "finds the code for the no-requirements case" $ do
      evalExpression cart (Is "this coupon code")
        `shouldBe`
        Just "this coupon code"

    describe "Has Code" $ do

      it "finds the code if it matches the has code rule" $ do
        evalExpression
          (cart{ codes=["bike-coupon"] })
          (Has (Code "bike-coupon") $ Is "bike-coupon")
        `shouldBe`
          Just "bike-coupon"

      it "finds nothing if it doesn't match the has code rule" $ do
        evalExpression
          cart
          (Has (Code "bike-coupon") $ Is "bike-coupon")
        `shouldBe`
          Nothing

    describe "Has One / Has Two" $ do

      it "finds the code for a single item" $ do
        -- TODO: remove :: Cart once on GHC 9
        let cart' = cart{ items=[Item { slug="car" }] } :: Cart
        evalExpression cart' (Has (One "car") $ Is "car-coupon")
          `shouldBe`
          Just "car-coupon"

      it "finds the code for two items" $ do
        let item = Item { slug="hat" }
        evalExpression (cart{ items=[item, item] }) (Has (Two "hat") $ Is "hat-coupon")
          `shouldBe`
          Just "hat-coupon"

      it "finds nothing for item rule miss" $ do
        evalExpression cart (Has (One "car") $ Is "car-coupon")
          `shouldBe`
          Nothing

        evalExpression cart (Has (Two "car") $ Is "car-coupon")
          `shouldBe`
          Nothing

    describe "In" $ do

      it "finds the code for a country place rule" $ do
        evalExpression
          cart{ location=Just"MX" }
          (In [Country "MX"] $ Is "mx-coupon")
        `shouldBe`
          Just "mx-coupon"

      it "finds the code if one of the countries match" $ do
        evalExpression
          cart{ location=Just "GB" }
          (In [Country "BR", Country "GB"] $ Is "gb-or-br-coupon")
        `shouldBe`
          Just "gb-or-br-coupon"

      it "finds nothing if no country places match" $ do
        evalExpression
          cart{ location=Just "US" }
          (In [Country "BR"] $ Is "visit brazil")
        `shouldBe`
          Nothing

      it "finds nothing if no location is in the cart" $ do
        evalExpression
          cart
          (In [Country "US"] $ Is "us-coupon")
        `shouldBe`
          Nothing

    describe "OneOf" $ do

      it "find the code if a OneOf rule passes" $ do
        evalExpression
          cart{ items=[Item { slug="boat" }] }
          (OneOf [Has (One "car") $ Is "", Has (One "boat") $ Is ""] $ Is "car-or-boat")
        `shouldBe`
          Just "car-or-boat"

      it "finds nothing if no rules pass" $ do
        evalExpression
          cart
          (OneOf [Has (One "car") $ Is "", Has (One "boat") $ Is ""] $ Is "car-or-boat")
        `shouldBe`
          Nothing

    describe "Between" $ do

      currentTime <- runIO now

      it "matches if the current time is in range" $ do
        let
          future = add day currentTime
          past = add (invert day) currentTime

        evalExpression
          cart{ time=currentTime }
          (Between past future $ Is "the most now code")
        `shouldBe`
          Just "the most now code"

      it "finds nothing if not in range" $ do
        let
          future = add day currentTime
          past = add (invert day) currentTime

        evalExpression
          cart -- since we initalized it with 1989 as the year
          (Between past future $ Is "the most now code")
        `shouldBe`
          Nothing

  describe "matchCoupons" $ do

    let
      emptyState = RuleState { rules=[], closedRules=[] }
      run' = runM . runLogIO . evalState emptyState . runRuleRepo
      closedRule = Rule (Has (Code "free car") $ Is "Free Car") [FreeProduct "car"]
      openRule = Rule (In [Country "BR"] $ Is "BR Only") [AmountOff 50 WholeCart]

    it "returns an empty list if nothing matched" $ do
      actions <- run' $ do
        addRule closedRule
        addRule openRule

        matchCoupons cart

      actions `shouldBe` Right []

    it "returns the closed rule if it matches" $ do
      actions <- run' $ do
        addRule closedRule
        addRule openRule

        matchCoupons (cart { codes=["free car"] })

      actions `shouldBe` Right [("Free Car", [FreeProduct "car"])]

    it "returns the open rules if it matches" $ do
      actions <- run' $ do
        addRule closedRule
        addRule openRule

        matchCoupons (cart { location=Just "BR" })

      actions `shouldBe` Right [("BR Only", [AmountOff 50 WholeCart])]

    it "returns both rules if both match" $ do
      actions <- run' $ do
        addRule closedRule
        addRule openRule

        matchCoupons (cart { location=Just "BR", codes=["free car"] })

      actions
        `shouldBe`
        Right [ ("BR Only", [AmountOff 50 WholeCart])
              , ("Free Car", [FreeProduct "car"])
              ]



-- this is just for the repl
main = hspec spec
