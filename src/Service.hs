{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Service
  -- all we expose for the real API
  ( Context
  , runGetActions
  -- exported for the tests
  , getActions
  , findAllSlugs
  , evalExpression
  ) where

import Prelude hiding (log)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Algebra (Has)
import Control.Carrier.Reader
import Control.Carrier.Lift
import Effects.Logging (Log, log, runLogIO)

import Data.List
import Data.Maybe

import Data.Aeson

import Database.PostgreSQL.Simple (Connection)

import Domain.Context
import Domain.RuleRepository
import Domain.Rule
import Domain.Shared

findAllSlugs :: Context -> [Slug]
findAllSlugs Context{ items, bundles } =
  let collectSlugs = fmap #slug
      bundleItemSlugs = fmap (collectSlugs . #items) bundles
      bundleSlugs = fmap #slug bundles
      itemSlugs = collectSlugs items
  in itemSlugs <> concat bundleItemSlugs <> bundleSlugs

evalExpression :: Context -> Expression -> Maybe Code
evalExpression context expr = case expr of
  -- base case or goal, if all rules match then it gets to the rule's Name
  Is code -> Just code

  Has (Code code) expr ->
    if code `elem` codes context then
      -- it passed this rule, so we continue evaluating
      evalExpression context expr
    else
      -- it didn't have the code, so we stop evaluating
      Nothing

  Has (One slug) expr ->
    if slug `elem` findAllSlugs context then
      evalExpression context expr
    else
      Nothing

  Has (Two slug) expr ->
    if length (filter (== slug) (findAllSlugs context)) >= 2 then
      evalExpression context expr
    else
      Nothing

  In places expr ->
    case location context of
      Nothing -> Nothing
      Just place ->
        -- here we're saying that if location is in the context,
        -- treat it as a country (since that's all this supports for now)
        let country = Country place
        in
          if country `elem` places then
            evalExpression context expr
          else
            Nothing

  OneOf exprs expr ->
    -- extremely cool, though I wonder about ignoring the codes
    -- from sub-rules in OneOf.  If we returned them, could you
    -- do something cool?
    if any isJust (fmap (evalExpression context) exprs) then
      evalExpression context expr
    else
      Nothing

  Between start end expr ->
    -- also cool, if it's equal after sorting it's in range
    if [start, time context, end] == sort [start, time context, end] then
      evalExpression context expr
    else
      Nothing

evalRule :: Context -> Rule -> Maybe (Code, [Action])
evalRule context (Rule expression action) =
  case evalExpression context expression of
    Just code -> Just (code, action)
    Nothing   -> Nothing

filterDNEs :: [Either RepoError Rule] -> [Either RepoError Rule]
filterDNEs = filter (/= Left DoesNotExist)

getActions ::
  ( Has Log sig m
  , Has RuleRepo sig m
  ) => Context -> m (Either RepoError [(Code, [Action])])
getActions context = do
  openRules <- getOpenRules
  closedRules <- sequence . filterDNEs <$> mapM getClosedRule (codes context)

  let
    -- since we want to combine the inner rules of the Eithers (openRules & closedRules),
    -- we do it this way.  written without using this pattern looks like
    -- combined' = fmap (\closed -> (fmap (\open -> open <> closed) openRules)) closedRules
    combined = (<>) <$> openRules <*> closedRules
    matchedRules = mapMaybe (evalRule context) <$> combined

  pure matchedRules

runGetActions :: MonadIO m => Connection -> Context -> m (Either RepoError [(Code, [Action])])
runGetActions conn context =
  runM
  . runLogIO
  . runReader conn
  . runRuleRepoIO
  $ getActions context
