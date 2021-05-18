{-# LANGUAGE OverloadedLabels #-}

module Service where

import Prelude hiding (log)

import Control.Algebra (Has)
import Control.Carrier.Lift
import Effects.Logging (Log, log, runLogIO)

import Domain.Cart
import Domain.RuleRepository

import Data.Aeson

getDiscounts ::
    ( Has Log sig m
    , Has RuleRepo sig m
    )
  => Cart -> [CouponCode] -> m [Discount]
getDiscounts cart codes = do
  log $ "Finding discounts for cart " <> #id cart <> " and coupon codes " <> show codes

  log . show =<< getRules

  pure []

createDiscount :: Item -> ()
createDiscount = undefined
