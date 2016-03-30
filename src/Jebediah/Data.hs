{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Data (
    Following (..)
  , GroupName (..)
  , StreamName (..)
  ) where

import           P

data Following =
  Follow Int
  | NoFollow
  deriving (Eq, Show)

newtype GroupName = GroupName { unGroupName :: Text } deriving (Eq, Show)
newtype StreamName = StreamName { unStreamName :: Text } deriving (Eq, Show)
