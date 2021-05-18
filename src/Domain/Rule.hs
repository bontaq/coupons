{-# LANGUAGE DeriveGeneric #-}
module Domain.Rule where

import GHC.Generics

type StartDate = Integer
type EndDate = Integer
type MinAmount = Integer
type Slug = String

data Expression
  = BigSpend  MinAmount         Expression
  | DateRange StartDate EndDate Expression
  | HasCoupon String            Expression
  | HasItem   MinAmount Slug    Expression
  | Locale    String            Expression
  -- Just for a bit of fun, allows expression of
  -- OneOf [Locale "USA" (Name "US"), Locale "DE" (Name "Germany")] (Name "US-or-Germany")
  | OneOf     [Expression]      Expression
  -- nicely requires any ruleset to end in name
  | Name      String
  deriving (Generic, Show, Eq)

data Target
  = Slug String
  | WholeCart
  deriving (Generic, Show, Eq)

data Result
  = AmountOff    Integer Target
  | PercentOff   Integer Target
  | FreeProduct  Slug
  | ReferralUsed String -- eh why not
  deriving (Generic, Show, Eq)

data Rule = Rule Expression [Result]
  deriving (Generic, Show, Eq)

-- We can say:
-- Rule
--   (HasItem 2 "bike" (Name "M-multiple bikes"))
--   [AmountOff 150 WholeCart]
-- or
-- Rule
--   Locale USA (DateRange (May 16) (May 22) (HasItem "bike" (Name "People like hats")))
--   [FreeProduct "hat"]
-- or
-- Rule
--   HasCoupon "sakib42" (Name "sakib42")
--   [AmountOff 50 WholeCart]
-- or
-- Rule
--   DateRange mothers-day-start mothers-day-end (HasItem "bike-package" (Name "Mother's day"))
--   [AmountOff 50 (Slug "shoes"), FreeProduct "mat"]
