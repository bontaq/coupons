{-# LANGUAGE OverloadedLabels #-}

module Service where

import Prelude hiding (log)

import Control.Algebra (Has)
import Control.Carrier.Lift
import Effects.Logging (Log, log, runLogIO)

import Models

import Data.Aeson

-- Has Logger
-- Has Repo

getDiscounts :: Has Log sig m => Cart -> [CouponCode] -> m [Discount]
getDiscounts cart codes = do
  log $ "Finding discounts for cart " <> #id cart <> " and coupon codes " <> show codes

  pure []

createDiscount :: Item -> ()
createDiscount = undefined
