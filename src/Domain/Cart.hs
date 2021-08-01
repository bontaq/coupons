{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Cart where

-- TODO: disappear this once this is on GHC 9.6, which has the glories record selectors
-- these imports and the HasField... stuff below is just to have a nicer way of getting
-- things off the cart without lens,
-- i.e. fmap #slug (cart #items) to get all the slugs of items in the cart
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

-- TODO this is used by the API because we want time
-- to come from the server (since someone could fake a time)
-- but I don't think it belongs here, and I'm not sure where it
-- should go.  a mappers folder or something?
data CartWithoutTime = CartWithoutTime
  { items    :: [Item]
  , bundles  :: [Bundle]
  , location :: Maybe String
  , codes    :: [Code]
  } deriving (Generic, Show)

instance FromJSON Cart where
instance FromJSON CartWithoutTime where
instance FromJSON Item where
instance FromJSON Bundle where
