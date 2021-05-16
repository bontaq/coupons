{-# LANGUAGE DeriveFunctor, KindSignatures, GADTs, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Effects.Logging
  ( Log, log, runLogIO, runLogFile )
where

import Prelude hiding (log)

-- Fused effects things
import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind (Type)

-- Fast logger
import System.Log.FastLogger

-- Alright, here goes.  The first thing we need is a logging effect.
-- Thankfully there is an example of this in fused-effects.
-- Don't be afraid!
-- REMAIN CALM!!!

data Message
  = Debug String
  | Info  String
  | Error String
  | Warn  String

-- Render a structured log message as a string.
renderLogMessage :: Message -> String
renderLogMessage message = case message of
  Error message -> "[error] " ++ message
  Debug message -> "[debug] " ++ message
  Info  message -> "[info] "  ++ message
  Warn  message -> "[warn] "  ++ message

--
-- The logging effect
--
-- This is the type definition and a shorthand way to actually use
-- the effect.  It doesn't include a way to actually interpret / run
-- the effect!

data Log (m :: Type -> Type) k where
  Write :: String -> Log m ()

log' :: Has Log sig m => Message -> m ()
log' message = send (Write $ renderLogMessage message)

log :: Has Log sig m => String -> m ()
log message = log' (Info message)

logInfo :: Has Log sig m => String -> m ()
logInfo message = log' (Info message)

logWarn :: Has Log sig m => String -> m ()
logWarn message = log' (Warn message)

logError :: Has Log sig m => String -> m ()
logError message = log' (Error message)

logDebug :: Has Log sig m => String -> m ()
logDebug message = log' (Debug message)

--
-- The logging effect carriers
--
-- This is where we define the actual interpretation of the logging effect.
-- Fused effects calls these "effect carriers" which is a little odd but
-- we may as well use the same language as them.
--

newtype LogIO m a = LogIO
  { runLogIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra  sig m) => Algebra (Log :+: sig) (LogIO m) where
  alg handle sig context = case sig of
    L (Write msg) -> context <$ liftIO (print msg)
    R other       -> LogIO (alg (runLogIO . handle) other context)


data LogWrapper = LogWrapper

newtype LogFileCarrier m a = LogFileCarrier (FastLogger -> m a)
  deriving (Functor)

instance Functor m => Applicative (LogFileCarrier m) where

instance Functor m => Monad (LogFileCarrier m) where

runLogFile :: FastLogger -> LogFileCarrier m a -> m a
runLogFile logger (LogFileCarrier runLogCarrier) = runLogCarrier logger


instance (MonadIO m, Algebra  sig m) => Algebra (Log :+: sig) (LogFileCarrier m) where
  alg handle sig context = LogFileCarrier $ \logger -> case sig of
    L (Write msg) -> context <$ liftIO (logger $ toLogStr msg)
    R other       -> alg (runLogFile logger . handle) other context


--
-- Example usage
--
application :: Has Log sig m => m ()
application = do
  logInfo "hello"

main :: IO ()
main = do
  (logger, cleanup) <- newFastLogger (LogStdout defaultBufSize)

  runM
    . runLogFile logger
    $ application
