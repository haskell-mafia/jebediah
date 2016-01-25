{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Data (
    Following (..)
  , GroupName (..)
  , StreamName (..)
  , BatchSize (..)
  , DiffList
  , fromDiff
  , toDiff
  ) where

import           P

import           Data.Text

data Following =
  Follow !Int
  | NoFollow
  deriving (Eq, Show)

data BatchSize =
  BatchSize !Int !Int !Rational -- Number, Size, TimeDiff (seconds)
  deriving (Eq, Show)

-- A CPS list concatenation
type DiffList a = [a] -> [a]

fromDiff :: DiffList a -> [a]
fromDiff x = x []

toDiff :: [a] -> DiffList a
toDiff l = \t -> l <> t

newtype GroupName = GroupName { unGroupName :: Text } deriving (Eq, Show)
newtype StreamName = StreamName { unStreamName :: Text } deriving (Eq, Show)
