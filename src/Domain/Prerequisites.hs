{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Prerequisites where

import GHC.Records
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)

import Domain.Shared

instance forall x r a. HasField x r a => IsLabel x (r -> a) where
  fromLabel r = getField @x r

data Bundle = Bundle
  { bundleItems :: [Item]
  , slug  :: String
  }

data Item = Item
  { slug :: String
  }

data Prerequisites = Prerequisites
  { items    :: [Item]
  , bundles  :: [Bundle]
  , location :: Maybe String
  , codes    :: [Code]
  }

