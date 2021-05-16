-- | Getting and adding coupons

module Repository where

import Models

type StartDate = Integer
type EndDate = Integer
type MinAmount = Integer

data RuleConstructor
  = BigSpend  MinAmount         RuleConstructor
  | DateRange StartDate EndDate RuleConstructor
  | HasCoupon String            RuleConstructor
  | HasItem   MinAmount Sku     RuleConstructor
  | Locale    Currency          RuleConstructor
  | Name      String -- nicely requires any ruleset to end in name
  deriving Show

data Target
  = Slug String
  | WholeCart

data RuleEffect
  = AmountOff Integer Target
  | PercentOff Integer Target
  | FreeProduct Sku

data Rule = Rule RuleConstructor [RuleEffect]

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

-- getCouponsForSku :: Sku -> [Coupon]
getCouponsForSku = undefined
