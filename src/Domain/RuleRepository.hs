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
import Database.PostgreSQL.Simple
import Data.ByteString.Char8 hiding (concat)

-- JSON handling
import Data.Aeson

-- Helper
import Data.Maybe

--
-- I guess you would call this the mapping layer so that we
-- can transform our rules into and out of JSON for storing
-- as jsonb fields
--
instance ToJSON Expression where
instance FromJSON Expression where

instance ToJSON Target where
instance FromJSON Target where

instance ToJSON Action where
instance FromJSON Action where

instance ToJSON Rule where
instance FromJSON Rule where


--------------------------------
-- The API for clients to use --
--------------------------------

addRule :: Has RuleRepo sig m => Rule -> m ()
addRule rule = send (AddRule rule)

getRules :: Has RuleRepo sig m => m (Either Error [Rule])
getRules = send GetOpenRules


----------------------------------------------------------
-- The Effect definition, in this case a repo for rules --
----------------------------------------------------------

data RuleRepo (m :: Type -> Type) k where
  AddRule       :: Rule -> RuleRepo m ()
  GetOpenRules  :: RuleRepo m (Either Error [Rule])
  GetClosedRule :: Code -> RuleRepo m (Either Error Rule)


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

parse :: FromJSON a => Value -> Either Error a
parse = toEither . fromJSON

handleToRule :: (Value, Value) -> Either Error Rule
handleToRule (expression, result) =
  Rule <$> parse expression <*> parse result

getCode :: Expression -> Maybe [Code]
getCode expr = case expr of
  (HasCode code _)     -> Just [code]
  (DateRange _ _ expr) -> getCode expr
  (MinSpend _ expr)    -> getCode expr
  (HasItem _ _ expr)   -> getCode expr
  (Locale _ expr)      -> getCode expr
  (OneOf exprs expr)   ->
    -- TODO: this should be more elegant
    -- collect any codes from exprs or expr
    let
      possibleCodes = concat $ mapMaybe getCode (expr:exprs)
    in
      case possibleCodes of
        [] -> Nothing
        _  -> Just possibleCodes
  Name _               -> Nothing

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

      L (GetClosedRule code) -> do
        rawRows <-
          liftIO (query conn
                   "select expression, result from closed_rules where code = ?"
                   (Only code) :: IO [(Value, Value)])

        let result = fmap handleToRule rawRows

        case result of
          [rule] -> pure $ rule <$ ctx
          _      -> pure $ Left "Too many results" <$ ctx

      L (AddRule (Rule expression result)) -> do
        liftIO $
          execute conn
            "insert into rules (expression, result) values (?, ?)"
            (encode expression, encode result)  -- turn them into JSON

        ctx <$ pure ()

      R other -> alg (runRuleRepoIO . handle) (R other) ctx  -- hand off to other interpreters


----------------------------------------------------------------
-- A Pure Effect interpreter that just uses a Record and List --
----------------------------------------------------------------

data RuleState = RuleState { rules :: [Rule], closedRules :: [Rule] }
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

    L (AddRule newRule) -> do
      oldRules <- rules <$> get

      -- set the state with the new rule at front
      put $ RuleState { rules = newRule : oldRules, closedRules = [] }

      -- TODO: I should return an Either Error Rule for adding a Rule
      ctx <$ pure ()

    L (GetClosedRule code) -> do
      rules <- closedRules <$> get

      -- gotta do a filter


      case rules of
        [rule] -> pure $ Right rule <$ ctx
        _      -> pure $ Left "Too many results" <$ ctx

    R other -> alg (runRuleRepo . handle) (R other) ctx


---------------------
-- Just some cruft --
---------------------

-- query :: [Rule]
-- query' = do
--   conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"
--   i <- query_ conn "select expression, result from rules" :: IO [(Value, Value)]
--   print i
--
-- -- insert :: Rule -> IO ()
-- insert' = do
--   conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"
--   let expression = encode $ BigSpend 500 (Name "Big Spenda")
--       result = encode $ [AmountOff 1000 WholeCart]
--
--   execute conn
--     "insert into rules (expression, result) values (?, ?)"
--     (expression, result)
--   pure ()
--
--
-- -- getCouponsForSku :: Sku -> [Coupon]
--
-- getCouponsForSku = undefined
