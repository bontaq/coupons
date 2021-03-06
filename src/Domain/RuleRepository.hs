{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, KindSignatures, GADTs, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Domain.RuleRepository where

import Domain.Rule
import Domain.Shared

-- Fused Effects
import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind (Type)

-- Database
import Database.PostgreSQL.Simple hiding (In)
import qualified Database.PostgreSQL.Simple as PS
import Data.ByteString.Char8 hiding (concat, concatMap, filter, elem)

-- JSON handling
import Data.Aeson

-- Helper
import Data.Maybe



--------------------------------
-- The API for clients to use --
--------------------------------

addRule :: Has RuleRepo sig m => Rule -> m (Either RepoError Rule)
addRule rule = send (AddRule rule)

getRules :: Has RuleRepo sig m => [Code] -> m (Either RepoError [Rule])
getRules codes = do
  openRules <- send GetOpenRules
  closedRules <- send (GetClosedRules codes)
  pure (openRules <> closedRules)

getOpenRules :: Has RuleRepo sig m => m (Either RepoError [Rule])
getOpenRules = send GetOpenRules

getClosedRules :: Has RuleRepo sig m => [Code] -> m (Either RepoError [Rule])
getClosedRules codes = send (GetClosedRules codes)


----------------------------------------------------------
-- The Effect definition, in this case a repo for rules --
----------------------------------------------------------

data RuleRepo (m :: Type -> Type) k where
  AddRule        :: Rule -> RuleRepo m (Either RepoError Rule)
  GetOpenRules   :: RuleRepo m (Either RepoError [Rule])
  GetClosedRules :: [Code] -> RuleRepo m (Either RepoError [Rule])


--------------------------------------------------
-- An Effect interpreter that talks to Postgres --
--------------------------------------------------

newtype RuleRepoIO m a = RuleRepoIO
  { runRuleRepoIO :: (ReaderC Connection m) a }
  deriving (Applicative, Functor, Monad, MonadIO)

--
-- Helpers
--
toEither :: Result a -> Either Error a
toEither result = case result of
  Success a -> Right a
  Error str -> Left str

parse :: FromJSON a => Value -> Either RepoError a
parse value =
  case toEither . fromJSON $ value of
    Left err -> Left (FailedToParseRule err)
    Right v  -> Right v

handleToRule :: (Value, Value) -> Either RepoError Rule
handleToRule (expression, result) =
  Rule <$> parse expression <*> parse result

-- TODO: could this be cleaned up with a traversable definition?
getCodes :: Expression -> [Code]
getCodes expr = case expr of
  (Has (Code code) _) -> [code]
  (Has _ expr)        -> getCodes expr
  (Between _ _ expr)  -> getCodes expr
  (In _ expr)         -> getCodes expr
  (OneOf exprs expr)  -> concatMap getCodes (expr : exprs)
  Is _                -> []

--
-- Actually specifying how to handle the effect definition and talking to postgres
--
instance (MonadIO m, Algebra sig m) => Algebra (RuleRepo :+: sig) (RuleRepoIO m) where
  alg handle sig ctx = RuleRepoIO $ do
    -- it expects a connection from a reader monad -- look at the tests, it's not too wild
    -- and allows us to do weirder things than if we got the connection here
    conn <- ask @Connection

    -- handling interpretation of messages from the definition
    case sig of
      L GetOpenRules -> do
        -- the query and result from postgres
        -- Value is how Aeson represents JSON
        rawRows <- liftIO
          (query_ conn
            "select expression, result from rules" :: IO [(Value, Value)])

        -- turning the (Value, Value) into Either Error [Rule]
        let result = mapM handleToRule rawRows

        -- replace the inside of ctx with result, and rewrap it into the monad
        pure $ result <$ ctx

      L (GetClosedRules codes) -> do
        rawRows <-
          liftIO (query conn
                   "select expression, result from closed_rules where code in ?"
                   (Only $ PS.In codes) :: IO [(Value, Value)])

        let result = mapM handleToRule rawRows

        pure $ result <$ ctx

      L (AddRule (Rule expression result)) -> do
        -- if the rule expression has codes, we put it into closed_rules
        let codes = getCodes expression

        case codes of
          [] ->
            liftIO $
              execute conn
                "insert into rules (expression, result) values (?, ?)"
                (encode expression, encode result)  -- turn them into JSON
          codes ->
            liftIO $ executeMany conn
              "insert into closed_rules (expression, result, code) values (?, ?, ?)"
              (fmap (encode expression, encode result,) codes)

        pure $ Right (Rule expression result) <$ ctx

      R other -> alg (runRuleRepoIO . handle) (R other) ctx  -- hand off to other interpreters


----------------------------------------------------------------
-- A Pure Effect interpreter that just uses a Record and List --
----------------------------------------------------------------

data RuleState = RuleState { rules :: [Rule], closedRules :: [(Rule, Code)] }
  deriving (Eq, Show)

newtype RuleRepoState m a = RuleRepoState
  { runRuleRepo :: (StateC RuleState m) a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra sig m => Algebra (RuleRepo :+: sig) (RuleRepoState m) where
  alg handle sig ctx = RuleRepoState $ case sig of
    L GetOpenRules -> do
      rules <- rules <$> get
      -- Remember it returns an Either, this is just always Right
      pure $ Right rules <$ ctx

    L (AddRule newRule@(Rule expression actions)) -> do

      let codes = getCodes expression
      case codes of
        [] -> do
          -- set the state with the new rule at front
          modify (\RuleState { rules, closedRules } ->
                    RuleState { rules=newRule:rules, closedRules=closedRules })
        codes' -> do
          let newRules = fmap (newRule,) codes'
          modify (\RuleState { rules, closedRules } ->
                    RuleState { rules=rules, closedRules=newRules <> closedRules })

      -- TODO: I should return an Either Error Rule for adding a Rule
      pure $ Right newRule <$ ctx

    L (GetClosedRules codes) -> do
      rulesWithCodes <- filter (\(_, code') -> code' `elem` codes) . closedRules <$> get

      let rules = fmap fst rulesWithCodes

      pure $ Right rules <$ ctx

    R other -> alg (runRuleRepo . handle) (R other) ctx
