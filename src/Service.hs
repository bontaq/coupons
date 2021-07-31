{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Service
  -- all we expose for the real API
  ( Cart (..)
  , CartWithoutTime (..)
  , runMatchCoupons
  -- exported for the tests
  , matchCoupons
  , findAllSlugs
  , evalExpression
  ) where

import Prelude hiding (log)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Algebra (Has)
import Control.Carrier.Reader
import Control.Carrier.Lift
import Effects.Logging (Log, log, logError, runLogger)
import System.Log.FastLogger

import Data.List
import Data.Maybe

import Data.Aeson

import Database.PostgreSQL.Simple (Connection)

import Domain.RuleRepository
import Domain.Rule
import Domain.Shared
import Domain.Cart

findAllSlugs :: Cart -> [Slug]
findAllSlugs Cart{ items, bundles } =
  let collectSlugs = fmap #slug

      -- pull all the slugs from cart.bundles, and items in the bundles
      bundleSlugs = fmap #slug bundles
      bundleItemSlugs = fmap (collectSlugs . #items) bundles

      -- pull the slugs off cart.items
      itemSlugs = collectSlugs items
  in itemSlugs <> concat bundleItemSlugs <> bundleSlugs

evalExpression :: Cart -> Expression -> Maybe Code
evalExpression cart expr = case expr of
  -- base case or goal, if all rules match then it gets to the rule's Name
  Is code -> Just code

  Has (Code code) expr ->
    if code `elem` #codes cart then
      -- it passed this rule, so we continue evaluating
      evalExpression cart expr
    else
      -- it didn't have the code, so we stop evaluating
      Nothing

  Has (One slug) expr ->
    if slug `elem` findAllSlugs cart then
      evalExpression cart expr
    else
      Nothing

  Has (Two slug) expr ->
    if length (filter (== slug) (findAllSlugs cart)) >= 2 then
      evalExpression cart expr
    else
      Nothing

  In places expr ->
    case #location cart of
      Nothing -> Nothing
      Just place ->
        -- here we're saying that if location is in the cart,
        -- treat it as a country (since that's all this supports for now)
        let country = Country place
        in
          if country `elem` places then
            evalExpression cart expr
          else
            Nothing

  OneOf exprs expr ->
    -- extremely cool, though I wonder about ignoring the codes
    -- from sub-rules in OneOf.  If we returned them, could you
    -- do something cool?
    if any isJust (fmap (evalExpression cart) exprs) then
      evalExpression cart expr
    else
      Nothing

  Between start end expr ->
    -- also cool, if it's equal after sorting it's in range
    if [start, time cart, end] == sort [start, time cart, end] then
      evalExpression cart expr
    else
      Nothing

evalRule :: Cart -> Rule -> Maybe (Code, [Action])
evalRule cart (Rule expression action) =
  case evalExpression cart expression of
    Just code -> Just (code, action)
    Nothing   -> Nothing

matchCoupons ::
  ( Has Log sig m
  , Has RuleRepo sig m
  ) => Cart -> m (Either RepoError [(Code, [Action])])
matchCoupons cart = do
  openRules <- getOpenRules
  closedRules <- getClosedRules (#codes cart)

  let
    -- since we want to combine the inner rules of the Eithers (openRules & closedRules),
    -- we do it this way.  written without using this pattern looks like
    -- combined' = fmap (\closed -> (fmap (\open -> open <> closed) openRules)) closedRules
    combined = (<>) <$> openRules <*> closedRules
    matchedRules = mapMaybe (evalRule cart) <$> combined

  case matchedRules of
    Left error  -> logError $ show error <> " while trying to get rules for cart: " <> show cart
    Right rules -> log $ "Found rules: " <> show rules <> " for cart: " <> show cart

  pure matchedRules

runMatchCoupons
  :: MonadIO m
  => Connection
  -> FastLogger
  -> Cart
  -> m (Either RepoError [(Code, [Action])])
runMatchCoupons conn logger cart =
  runM
  . runReader conn
  . runRuleRepoIO
  . runLogger logger
  $ matchCoupons cart
