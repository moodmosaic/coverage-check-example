module Test.Q72168364.QuickCheck (
    runner
  ) where

import Data.List (nub)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (checkCoverage, counterexample, cover, ioProperty, (===))

-- System under test.
import Q72168364 (rints)

-- defaultMain returns IO (),
-- but we'd rather want IO Bool.
import Control.Exception (catch)
import System.Exit (ExitCode (..))

runner :: IO Bool
runner =
  let
    tests :: [Test]
    tests =
      -- https://stackoverflow.com/a/72182415
      [ testProperty "Each integer is between 0 and 10" $ \() -> ioProperty $ do
          actual <- rints
          return $
            counterexample ("actual: " ++ show actual) $
            all (\i -> 0 <= i && i <= 10) actual

      , testProperty "The same number does not appear twice" $ \() -> ioProperty $ do
          actual <- rints
          return $ nub actual === actual

      , testProperty "Length is and distribution is correct" $ \() -> ioProperty $ do
          actual <- rints
          let l = length actual
          return $
            checkCoverage $
            cover 90 (l == 3) "Length 3" $
            cover 10 (l == 4) "Length 4"
            True -- Base property, but really, the distribution is the test

      , testProperty "3 appears 50% of the times" $ \() -> ioProperty $ do
          actual <- rints
          return $
            checkCoverage $
            cover 50 (3 `elem` actual) "3 present" $
            cover 50 (3 `notElem` actual) "3 absent"
            True -- Base property, but really, the distribution is the test

      , testProperty "All numbers are represented" $ \() -> ioProperty $ do
          actual <- rints
          return $
            checkCoverage $
            cover 5 ( 0 `elem` actual) " 0 present" $
            cover 5 ( 1 `elem` actual) " 1 present" $
            cover 5 ( 2 `elem` actual) " 2 present" $
            cover 5 ( 3 `elem` actual) " 3 present" $
            cover 5 ( 4 `elem` actual) " 4 present" $
            cover 5 ( 5 `elem` actual) " 5 present" $
            cover 5 ( 6 `elem` actual) " 6 present" $
            cover 5 ( 7 `elem` actual) " 7 present" $
            cover 5 ( 8 `elem` actual) " 8 present" $
            cover 5 ( 9 `elem` actual) " 9 present" $
            cover 5 (10 `elem` actual) "10 present"
            True -- Base property, but really, the distribution is the test
      ]
  in
    (defaultMain tests >> return True) `catch` (\exitCode ->
      if exitCode == ExitSuccess then
        return True
      else
        return False
    )
