{-# LANGUAGE DeriveGeneric #-}
module Domain.Rule where

import GHC.Generics

import Chronos
import Data.Aeson

import Domain.Shared

type StartDate = Time
type EndDate = Time
type MinAmount = Integer
type Count = Integer
type Slug = String

data CodeOrItem
  = Code Code
  | One Slug
  | Two Slug
  deriving (Generic, Show, Eq)

newtype Place = Country String
  deriving (Generic, Show, Eq)

data Expression
  = Between StartDate EndDate Expression
  | Has     CodeOrItem        Expression
  | In      [Place]           Expression
  | OneOf   [Expression]      Expression
  | Is      Code
  deriving (Generic, Show, Eq)

-- Thoughts / examples:
-- Has (One "bike-package"), Has (Is "coupon-code")
-- Has (One "shoe") (In [Locale "US"] (Is "shoe-coupon"))
-- Between "may-16" "may-22"
--   (In [Locale "US", Locale "GB"] (Has (One "bikepackage") (Is "happy spring coupon")))

-- other expression ideas
-- - MinSpend
-- - Count (this would require some level of actual parsing,
--          ie it removes the item on success)
-- -

data Target
  = Slug String
  | WholeCart
  deriving (Generic, Show, Eq)

data Action
  = AmountOff    Integer Target
  | PercentOff   Integer Target
  | FreeProduct  Slug
  | ReferralUsed String -- eh why not
  deriving (Generic, Show, Eq)

data Rule = Rule Expression [Action]
  deriving (Generic, Show, Eq)

-- We can say:
-- Rule
--   (Has 2 "bike" (Name "M-multiple bikes"))
--   [AmountOff 150 WholeCart]
-- or
-- Rule
--   Locale USA (DateRange (May 16) (May 22) (Has 1 "bike" (Name "People like hats")))
--   [FreeProduct "hat"]
-- or
-- Rule
--   HasCode "test-99" (Name "test-99")
--   [AmountOff 50 WholeCart]
-- or
-- Rule
--   DateRange mothers-day-start mothers-day-end (Has 1 "bike-package" (Name "Mother's day"))
--   [AmountOff 50 (Slug "shoes"), FreeProduct "mat"]
--
-- I guess you would call this the mapping layer so that we
-- can transform our rules into and out of JSON for storing
-- as jsonb fields
--
instance ToJSON CodeOrItem where
instance FromJSON CodeOrItem where

instance ToJSON Place where
instance FromJSON Place where

instance ToJSON Expression where
instance FromJSON Expression where

instance ToJSON Target where
instance FromJSON Target where

instance ToJSON Action where
instance FromJSON Action where

instance ToJSON Rule where
instance FromJSON Rule where
