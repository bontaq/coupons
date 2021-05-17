{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Getting and adding coupons

module Repository where

import Models

-- Database Stuff
import Database.PostgreSQL.Simple
import GHC.Generics
import Data.Aeson hiding (Result)

type StartDate = Integer
type EndDate = Integer
type MinAmount = Integer

data Expression
  = BigSpend  MinAmount         Expression
  | DateRange StartDate EndDate Expression
  | HasCoupon String            Expression
  | HasItem   MinAmount Sku     Expression
  | Locale    String            Expression
  -- Just for a bit of fun, allows expression of
  -- OneOf [Locale "USA" (Name "US"), Locale "DE" (Name "Germany")] (Name "US-or-Germany")
  | OneOf     [Expression]      Expression
  -- nicely requires any ruleset to end in name
  | Name      String
  deriving (Generic, Show)

instance ToJSON Expression where
instance FromJSON Expression where

data Target
  = Slug String
  | WholeCart
  deriving (Generic, Show)

instance ToJSON Target where
instance FromJSON Target where

data Result
  = AmountOff    Integer Target
  | PercentOff   Integer Target
  | FreeProduct  Sku
  | ReferralUsed String -- eh why not
  deriving (Generic, Show)

instance ToJSON Result where
instance FromJSON Result where

data Rule = Rule Expression [Result]
  deriving (Generic, Show)

instance ToJSON Rule where
instance FromJSON Rule where

-- query :: [Rule]
query' = do
  conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"
  i <- query_ conn "select expression, result from rules" :: IO [(Value, Value)]
  print i

-- insert :: Rule -> IO ()
insert' = do
  conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"
  let expression = encode $ BigSpend 500 (Name "Big Spenda")
      result = encode $ [AmountOff 1000 WholeCart]

  execute conn
    "insert into rules (expression, result) values (?, ?)"
    (expression, result)
  pure ()

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
