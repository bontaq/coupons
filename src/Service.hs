{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Service where

import Prelude hiding (log)

import Control.Algebra (Has)
import Control.Carrier.Reader
import Control.Carrier.Lift
import Effects.Logging (Log, log, runLogIO)

import Data.List

import Data.Aeson
import Data.Maybe

import Domain.Prerequisites
import Domain.RuleRepository
import Domain.Rule
import Domain.Shared

import Database.PostgreSQL.Simple

evalExpression :: Prerequisites -> Expression -> Maybe Code
evalExpression prerequisites expr = case expr of
  -- base case or goal, if all rules match then it gets to the rule's Name
  Name code -> Just code
  Has (Code code) expr ->
    if code `elem` codes prerequisites then
      -- it passed this rule, so we continue evaluating
      evalExpression prerequisites expr
    else
      -- it didn't have the code, so we stop evaluating
      Nothing


-- evalRule :: Prerequisites -> Expression -> Maybe (Code, [Action])
-- evalRule p rule = do
--   print ""
--
-- evalRule _ (Rule (Name couponCode) actions) = Just (couponCode, actions)
-- evalRule p@Prerequisites { codes } (Rule (HasCode code rest) actions) =
--   case find (== code) codes of
--     Just _  -> Just (code, actions)
--     Nothing -> evalRule p (Rule rest actions)

-- getDiscounts ::
--     ( Has Log sig m
--     , Has RuleRepo sig m
--     )
--   => Prerequisites -> m [Action]
-- getDiscounts cart codes = do
--   log $ "Finding discounts for cart " <> #id cart <> " and coupon codes " <> show codes
--
--   rules <- getOpenRules
--
--   let actions = mapMaybe (evalRule cart) <$> rules
--
--   log . show =<< getOpenRules
--
--   pure []

-- runDiscounts cart coupons = do
--   conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"
--
--   runM
--     . runLogIO
--     . runReader conn
--     . runRuleRepoIO
--     $ getDiscounts cart coupons

createDiscount :: Item -> ()
createDiscount = undefined
