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

instance forall x r a. HasField x r a => IsLabel x (r -> a) where
  fromLabel r = getField @x r

type UUID = String

type Sku = String

data Currency = USD | CAD
  deriving Show

data Money = Money
  { currency :: Currency
  , amount   :: Integer
  }

data Discount = Discount
  { itemId    :: UUID
  , amountOff :: Money
  }

data ProductOption = ProductOption
  { id :: UUID
  , sku :: String
  }

data Bundle = Bundle
  { productOptions :: [ProductOption]
  }

data Item = Item
  { productOption :: ProductOption
  }

data Cart = Cart
  { id :: UUID
  , bundleOptionSet :: [Bundle]
  , itemSet :: [Item]
  , currency :: Currency
  }

type CouponCode = String
