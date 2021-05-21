module Domain.Shared where

type Code = String

type Error = String

data RepoError
  = DoesNotExist
  | DatabaseErr String
  | TooManyResults
  | FailedToParseRule String
  deriving (Show, Eq)
