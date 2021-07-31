{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Cart where

import GHC.Records
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)
import GHC.Generics

import Chronos
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

data Cart = Cart
  { items    :: [Item]
  , bundles  :: [Bundle]
  , location :: Maybe String
  , codes    :: [Code]
  , time     :: Time
  } deriving (Generic, Show)

instance FromJSON Cart where
instance FromJSON Item where
instance FromJSON Bundle where
