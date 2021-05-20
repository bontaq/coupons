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

import Domain.Shared

instance forall x r a. HasField x r a => IsLabel x (r -> a) where
  fromLabel r = getField @x r

data Bundle = Bundle
  { items :: [Item]
  , slug  :: String
  }

newtype Item = Item
  { slug :: String
  }

data Context = Context
  { items    :: [Item]
  , bundles  :: [Bundle]
  , location :: Maybe String
  , codes    :: [Code]
  }
