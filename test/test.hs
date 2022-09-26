import qualified Test.Q72168364.Hedgehog
import qualified Test.Q72168364.QuickCheck

import qualified Control.Monad
import qualified System.Exit

-- cabal test --test-show-details=streaming
main :: IO ()
main = do
  results <- sequence [
      Test.Q72168364.Hedgehog.runner
    , Test.Q72168364.QuickCheck.runner
    ]

  Control.Monad.unless (and results)
    System.Exit.exitFailure
