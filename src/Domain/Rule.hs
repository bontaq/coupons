{-# LANGUAGE DeriveGeneric #-}
module Domain.Rule where

import GHC.Generics

import Domain.Shared

type StartDate = Integer
type EndDate = Integer
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
--   (In [Locale "US", Locale "GB"] (Has (One "bike-package") (Is "happy spring coupon")))

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
--   HasCode "sakib42" (Name "sakib42")
--   [AmountOff 50 WholeCart]
-- or
-- Rule
--   DateRange mothers-day-start mothers-day-end (Has 1 "bike-package" (Name "Mother's day"))
--   [AmountOff 50 (Slug "shoes"), FreeProduct "mat"]
