{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Context where

import GHC.Records
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)
import GHC.Generics

import Data.Time.Clock
import Data.Aeson

import Domain.Shared

instance forall x r a. HasField x r a => IsLabel x (r -> a) where
  fromLabel r = getField @x r

data Bundle = Bundle
  { items :: [Item]
  , slug  :: String
  } deriving (Generic, Show)

newtype Item = Item
  { slug :: String
  } deriving (Generic, Show)

data Context = Context
  { items    :: [Item]
  , bundles  :: [Bundle]
  , location :: Maybe String
  , codes    :: [Code]
  , time     :: UTCTime
  } deriving (Generic, Show)

instance FromJSON Context where
instance FromJSON Item where
instance FromJSON Bundle where
