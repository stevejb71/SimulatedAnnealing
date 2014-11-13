module Main where

import Control.Monad.Random (evalRand)
import Data.Functor (fmap)
import Data.Maybe (fromMaybe)
import EightQueens (produceSolution)
import System.Environment (getArgs)
import System.Random (newStdGen)

main = do
    args <- getArgs
    let size = read (args !! 0) :: Int
    random <- newStdGen
    let result = evalRand (produceSolution size) random
    let stringResult = fromMaybe "No solution found" (fmap show result)
    putStrLn stringResult
