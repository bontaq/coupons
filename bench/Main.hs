module Main where

-- networking
import Network.Wreq

-- json parsing
import Data.Aeson


hit = do
  r <- postWith opts
    "https://accounts.spotify.com/api/token"


main :: IO ()
main = undefined
