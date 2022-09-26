{-# LANGUAGE OverloadedStrings #-}
module Test.Q72168364.Hedgehog (
    runner
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (nub)
import Hedgehog

-- System under test.
import Q72168364 (rints)

runner :: IO Bool
runner =
  checkParallel $ Group "Properties" [
    ("Each integer is between 0 and 10",
      property $ do
        actual <- liftIO rints
        assert $ all (\i -> 0 <= i && i <= 10) actual
    ),
    ("The same number does not appear twice",
      property $ do
        actual <- liftIO rints
        nub actual === actual
    ),
    ("Length is and distribution is correct",
      verifiedTermination . withConfidence (10 ^ (3 :: Int)) . property $ do
        actual <- liftIO rints
        let l = length actual
        cover 89.5 "Length 3" $ l == 3 -- Should be 90.
        cover  9.5 "Length 4" $ l == 4 -- Should be 10.
        -- See https://github.com/hedgehogqa/haskell-hedgehog/pull/288#discussion_r940676278
    ),
    ("3 appears 50% of the times",
      verifiedTermination . withConfidence (10 ^ (3 :: Int)) . property $ do
        actual <- liftIO rints
        cover 49.5 "3 present" (3 `elem`    actual) -- Should be 50.
        cover 49.5 "3 absent"  (3 `notElem` actual) -- Should be 50.
        -- See https://github.com/hedgehogqa/haskell-hedgehog/pull/288#discussion_r940676278
    ),
    ("All numbers are represented",
      verifiedTermination . withConfidence (10 ^ (3 :: Int)) . property $ do
        actual <- liftIO rints
        cover 5 " 0 present" ( 0 `elem` actual)
        cover 5 " 1 present" ( 1 `elem` actual)
        cover 5 " 2 present" ( 2 `elem` actual)
        cover 5 " 3 present" ( 3 `elem` actual)
        cover 5 " 4 present" ( 4 `elem` actual)
        cover 5 " 5 present" ( 5 `elem` actual)
        cover 5 " 6 present" ( 6 `elem` actual)
        cover 5 " 7 present" ( 7 `elem` actual)
        cover 5 " 8 present" ( 8 `elem` actual)
        cover 5 " 9 present" ( 9 `elem` actual)
        cover 5 "10 present" (10 `elem` actual)
    )
  ]
