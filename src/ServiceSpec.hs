{-# LANGUAGE DuplicateRecordFields #-}
module ServiceSpec where

import Test.Hspec

import Domain.Rule
import Domain.Context

import Service

spec = parallel $ do

  let context = Context
        { items = []
        , bundles = []
        , location = Nothing
        , codes = []
        }

  describe "findAllSlugs" $ do

    it "works fine with empty information" $ do
      findAllSlugs context `shouldBe` []

    it "finds the slugs in items" $ do
      findAllSlugs (context{ items=[Item { slug="cat" }] })
        `shouldBe` ["cat"]

    it "find the bundle's name slug" $ do
      findAllSlugs (context{ bundles=[Bundle { items=[], slug="a package" }] })
        `shouldBe` ["a package"]

    it "finds the slugs in bundles" $ do
      let item = Item { slug="cat" }

      findAllSlugs (context{ bundles=[Bundle { items=[item], slug="bundle" }] })
        `shouldBe` ["cat", "bundle"]

    it "works with bundles and items" $ do
      let item = Item { slug="cat-item" }
      let bundle = Bundle { items=[item { slug="bundle-item" }], slug="a-bundle" }

      findAllSlugs context{ bundles=[bundle], items=[item] }
        `shouldBe` ["cat-item", "bundle-item", "a-bundle"]

  describe "evalExpression" $ do

    -- TODO: Could I use property testing for this?  Not sure

    it "finds the code for the no-requirements case" $ do
      evalExpression context (Is "this coupon code")
        `shouldBe`
        Just "this coupon code"

    describe "Has Code" $ do

      it "finds the code if it matches the has code rule" $ do
        evalExpression
          (context{ codes=["bike-coupon"] })
          (Has (Code "bike-coupon") $ Is "bike-coupon")
        `shouldBe`
          Just "bike-coupon"

      it "finds nothing if it doesn't match the has code rule" $ do
        evalExpression
          context
          (Has (Code "bike-coupon") $ Is "bike-coupon")
        `shouldBe`
          Nothing

    describe "Has One / Has Two" $ do

      it "finds the code for a single item" $ do
        -- TODO: remove :: Context once on GHC 9
        let context' = context{ items=[Item { slug="car" }] } :: Context
        evalExpression context' (Has (One "car") $ Is "car-coupon")
          `shouldBe`
          Just "car-coupon"

      it "finds the code for two items" $ do
        let item = Item { slug="hat" }
        evalExpression (context{ items=[item, item] }) (Has (Two "hat") $ Is "hat-coupon")
          `shouldBe`
          Just "hat-coupon"

      it "finds nothing for item rule miss" $ do
        evalExpression context (Has (One "car") $ Is "car-coupon")
          `shouldBe`
          Nothing

        evalExpression context (Has (Two "car") $ Is "car-coupon")
          `shouldBe`
          Nothing

    describe "In" $ do

      it "finds the code for a country place rule" $ do
        evalExpression
          context{ location=Just"MX" }
          (In [Country "MX"] $ Is "mx-coupon")
        `shouldBe`
          Just "mx-coupon"

      it "finds the code if one of the countries match" $ do
        evalExpression
          context{ location=Just "GB" }
          (In [Country "BR", Country "GB"] $ Is "gb-or-br-coupon")
        `shouldBe`
          Just "gb-or-br-coupon"

      it "finds nothing if no country places match" $ do
        evalExpression
          context{ location=Just "US" }
          (In [Country "BR"] $ Is "visit brazil")
        `shouldBe`
          Nothing

      it "finds nothing if no location is in the context" $ do
        evalExpression
          context
          (In [Country "US"] $ Is "us-coupon")
        `shouldBe`
          Nothing

    describe "OneOf" $ do

      it "find the code if a OneOf rule passes" $ do
        evalExpression
          context{ items=[Item { slug="boat" }] }
          (OneOf [Has (One "car") $ Is "", Has (One "boat") $ Is ""] $ Is "car-or-boat")
        `shouldBe`
          Just "car-or-boat"

      it "finds nothing if no rules pass" $ do
        evalExpression
          context
          (OneOf [Has (One "car") $ Is "", Has (One "boat") $ Is ""] $ Is "car-or-boat")
        `shouldBe`
          Nothing


main = hspec spec
