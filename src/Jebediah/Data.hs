{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Data (
    Following (..)
  ) where

import           P

data Following =
  Follow Int
  | NoFollow
  deriving (Eq, Show)
