{-# LANGUAGE OverloadedStrings #-}
module Helpers.Gen where

import System.Random
import Database.PostgreSQL.Simple hiding (In)
import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar

import Domain.Rule
import Domain.Shared

dayDiff = secondsToNominalDiffTime (24 * 60 * 60)

todayUTC = UTCTime (fromGregorian 2021 5 21) 0

openRules :: [Rule]
openRules =
  [ Rule (In [Country "US"] $ Is "usa-code") []
  , Rule (In [Country "GB"] $ Is "gb-code") []
  , Rule (Has (One "car") $ Is "car-code") [AmountOff 100 WholeCart]
  , Rule (Has (One "dog") $ Is "dog-code") [AmountOff 150 WholeCart]
  , Rule (Has (Two "dog") $ Is "two-dog-code") [AmountOff 250 WholeCart]
  , Rule (OneOf [ Has (One "cat") (Is "cat-code")
                , Has (One "dog") (Is "dog-code")
                ] $ Is "cat-or-dog-code") [AmountOff 10 WholeCart]
  , Rule (Between todayUTC (addUTCTime dayDiff todayUTC) $ Is "today only") []
  ]

-- insert a bunch of rules
addClosedRules code = do
  conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"

  let
    expression = Has (Code code) $ Is code
    result = [AmountOff 1000 WholeCart]

  execute conn
    "insert into closed_rules (expression, result, code) values (?, ?, ?)"
    (encode expression, encode result, "" :: Code)

  pure ()

addOpenRules (Rule expression result) = do
  conn <- connectPostgreSQL "host=localhost dbname=coupon user=coupon password=password"

  execute conn
    "insert into rules (expression, result) values (?, ?)"
    (encode expression, encode result )

  pure ()

addRules = do
  -- add 10,000 closed rules
  mapM_ (addClosedRules . show) [0..10000]
  mapM_ addOpenRules openRules

  print "done."
