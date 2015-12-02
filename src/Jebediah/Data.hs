{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Data (
    Following (..)
  ) where

import           P

data Following =
  Follow
  | NoFollow
  deriving (Eq, Show)
