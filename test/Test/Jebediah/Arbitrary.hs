{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Jebediah.Arbitrary (
    Line (..)
  ) where

import qualified Data.Text as T

import           Disorder.Corpus

import           Jebediah.Data

import           Numeric.Natural (Natural)

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

newtype Line =
  Line {
      getContent :: Text
    } deriving (Eq, Show)

instance Arbitrary LogGroup where
  arbitrary =
    pure $ LogGroup "jebediah.testing.1"

instance Arbitrary LogStream where
  arbitrary = do
    m <- elements muppets
    n <- choose (0, 1000000 :: Int)
    pure . LogStream . mconcat $ [m, ".", T.pack . show $ n]

instance Arbitrary Line where
  arbitrary =
    frequency [
        (99, (Line . T.unwords) <$> listOf1 (elements weather))
      , (1, pure $ Line "")
      ]

instance Arbitrary Natural where
  arbitrary =
    fromInteger <$> choose (0, 1000000000)
