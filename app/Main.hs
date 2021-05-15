{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Main where

import Lib
import Prelude hiding (log)

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind (Type)

-- Alright, here goes.  The first thing we need is a logging effect.
-- Thankfully there is an example of this in fused-effects.
-- Don't be afraid!
-- REMAIN CALM!!!

data Message
  = Debug String
  | Info String

-- Render a structured log message as a string.
renderLogMessage :: Message -> String
renderLogMessage message = case message of
  Debug message -> "[debug] " ++ message
  Info  message -> "[info] "  ++ message



--------------------------------------------------------------------------------
-- The logging effect
--------------------------------------------------------------------------------

data Log (m :: Type -> Type) k where
  Write :: String -> Log m ()

log :: Has Log sig m => String -> m ()
log message = send (Write message)

--------------------------------------------------------------------------------
-- The logging effect carriers
--------------------------------------------------------------------------------

newtype LogIO m a = LogIO { runLogIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra  sig m) => Algebra (Log :+: sig) (LogIO m) where
  alg handle sig context = case sig of
    L (Write msg) -> context <$ liftIO (print msg)
    R other       -> LogIO (alg (runLogIO . handle) other context)

newtype LogFile m a = LogFile { runLogFile :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)



application :: Has Log sig m => m ()
application = do
  log "hello"

main :: IO ()
main =
  runM
  . runLogIO
  $ application
