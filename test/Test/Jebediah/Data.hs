{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Jebediah.Data where

import           Jebediah.Data

import           Numeric.Natural (Natural)

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Jebediah.Arbitrary ()

prop_utcToUnix :: Natural -> Property
prop_utcToUnix time =
  (utcToUnix . unixToUtc . utcToUnix . unixToUtc $ time) === (utcToUnix . unixToUtc $ time)

prop_fudge :: [Log] -> Property
prop_fudge l =
  let
    fudged = fudge l
  in
   counterexample (mconcat ["Not in reverse-chronological order: ", show fudged]) $
     snd $ foldr (\el (last, acc) -> (Just el, acc && maybe True (\last' -> logTime el >= logTime last') last)) (Nothing, True) fudged

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000})
