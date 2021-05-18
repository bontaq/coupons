{-# LANGUAGE OverloadedLabels #-}

module Service where

import Prelude hiding (log)

import Control.Algebra (Has)
import Control.Carrier.Lift
import Effects.Logging (Log, log, runLogIO)

import Data.Aeson
import Data.Maybe

import Domain.Cart
import Domain.RuleRepository
import Domain.Rule


referralCheck :: Has RuleRepo sig m => CouponCode -> m (Maybe [Action])
referralCheck = undefined

evalRule :: Cart -> Rule -> Maybe (CouponCode, [Action])
evalRule = undefined

getDiscounts ::
    ( Has Log sig m
    , Has RuleRepo sig m
    )
  => Cart -> [CouponCode] -> m [Action]
getDiscounts cart codes = do
  log $ "Finding discounts for cart " <> #id cart <> " and coupon codes " <> show codes

  rules <- getRules

  let actions = mapMaybe (evalRule cart) <$> rules

  log . show =<< getRules

  pure []

createDiscount :: Item -> ()
createDiscount = undefined
