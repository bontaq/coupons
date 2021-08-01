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

import Control.Monad.IO.Class (MonadIO(..))
import Control.Carrier.Reader
import Control.Carrier.Lift
import Database.PostgreSQL.Simple (Connection)
import System.Log.FastLogger

import Effects.Logging (Log, log, logError, runLogger)
import Domain.RuleRepository
import Domain.Rule
import Domain.Shared
import Domain.Cart

import Commands.MatchCoupons

--
-- The service just takes commands and hooks them up to be runnable
--

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
