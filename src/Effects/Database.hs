{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, KindSignatures, GADTs, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Effects.Database where

-- Fused effects things
import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind (Type)

-- Postgreql-simple
import Database.PostgreSQL.Simple
import Data.ByteString.Char8
import Data.Aeson
import Data.Aeson.Parser

--
-- The database effect
--
-- This is what we'll provide to the repository to run queries
--

-- data Store (m :: Type -> Type) k where
--   Query :: String -> Store m ()
--   Insert :: String -> Store m ()

-- fetch :: Has Store

-- select
import Repository

data RuleRepo (m :: Type -> Type) k where
  AddRule    :: Rule -> RuleRepo m ()
  UpdateRule :: Rule -> Rule -> RuleRepo m ()
  GetRules   :: RuleRepo m (Either String [Rule])

addRule :: Has RuleRepo sig m => Rule -> m ()
addRule rule = send (AddRule rule)

updateRule :: Has RuleRepo sig m => Rule -> Rule -> m ()
updateRule oldRule newRule = send (UpdateRule oldRule newRule)

getRules :: Has RuleRepo sig m => m (Either String [Rule])
getRules = send GetRules

toEither :: Data.Aeson.Result a -> Either String a
toEither result = case result of
  Success a -> Right a
  Error str -> Left str

parse :: FromJSON a => Value -> Either String a
parse = toEither . fromJSON

handleToRule :: (Value, Value) -> Either String Rule
handleToRule (expression, result) =
  Rule <$> parse expression <*> parse result

newtype RuleRepoIO m a = RuleRepoIO
  { runRuleRepoIO :: (ReaderC ByteString m) a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (RuleRepo :+: sig) (RuleRepoIO m) where
  alg handle sig ctx = RuleRepoIO $ case sig of
    L GetRules -> do
      psqlConnectionString <- ask
      conn <- liftIO $ connectPostgreSQL psqlConnectionString

      rawRows <- liftIO (query_ conn "select expression, result from rules" :: IO [(Value, Value)])

      let result = mapM handleToRule rawRows
      pure $ result <$ ctx


--
-- Example usage
--
application :: Has RuleRepo sig m => m (Either String [Rule])
application = do
  getRules

main :: IO ()
main = do
  rules <- runM
    . runReader "host=localhost dbname=coupon user=coupon password=password"
    . runRuleRepoIO
    $ application

  print rules
