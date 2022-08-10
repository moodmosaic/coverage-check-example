-- https://stackoverflow.com/a/72182415
module Q72168364 (
    rints
  ) where

import Control.Monad (forM)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import System.Random.Stateful (globalStdGen, uniformM, uniformRM)

rints :: IO [Int]
rints = do
  p <- uniformRM (1 :: Int, 100) globalStdGen
  let l = if 10 < p then 3 else 4
  ns <- shuffle $ [0..2] ++ [4..10]
  includeThree <- uniformM globalStdGen
  if includeThree
    then do
      let ns' = take (l - 1) ns
      shuffle $ 3 : ns'
    else
      return $ take l ns

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray l xs
  forM [1..l] $ \i -> do
      j <- uniformRM (i, l) globalStdGen
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
  where
    l = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n = newListArray (1, n)
