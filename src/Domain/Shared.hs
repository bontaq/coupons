module Domain.Shared where

type Error = String

data RepoError
  = DoesNotExist
  | DatabaseErr String
  | TooManyResults
  deriving (Show, Eq)
