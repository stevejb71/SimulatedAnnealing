{-# LANGUAGE ScopedTypeVariables #-}

module EightQueens where

import Annealing
import Control.Monad (join, liftM)
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Data.List (unfoldr)

newtype Board = Board [Int]

produceSolution :: (RandomGen r) => Int -> Rand r (Maybe Board)
produceSolution size = do
    board <- initialBoard size
    let temperatureValues = temperatures (Temperature 100.0) (Temperature 0.1) 0.99
    solution <- anneal board temperatureValues 1000
    return solution

instance Annealable Board where
    heat board = let s = size board - 1 in do
        x <- getRandomR (0, s)
        y <- getRandomR (0, s)
        let board' :: Board = swap board x y
        if (x == y) then heat board else return board'

    energy b@(Board bs) = fromIntegral $ sum $ map (\(c, r) -> (countConflictsToWest b r c (-1)) + (countConflictsToWest b r c 1)) (bs `zip` [0..])
        where countConflictsToWest :: Board -> Int -> Int -> Int -> Int
              countConflictsToWest b@(Board bs) row col dy = if (null diagonalPositions) then 0 else count (\(r, c) -> hasQueen b r c) (tail diagonalPositions)
                  where diagonalPositions :: [(Int, Int)] = unfoldr (\(r, c) -> if (r >= 0 && c >= 0 && r < size b) then Just $ ((r, c), (r + dy, c - 1)) else Nothing) (row, col)

instance Show Board where
    show b@(Board ps) = 
        let blankRow = replicate (size b) '.'
            rows = map (\i -> adjust (const 'Q') i blankRow) ps
        in join $ map (++ ['\n']) rows

size :: Board -> Int
size (Board ps) = length ps

swap :: Board -> Int -> Int -> Board
swap (Board bs) x y = 
    let bx = bs !! x
        by = bs !! y
        b1 = adjust (const bx) y bs
    in Board $ adjust (const by) x b1

hasQueen (Board ps) r c = (ps !! r) == c

clean size = Board [0..(size-1)]

initialBoard :: (RandomGen g) => Int -> Rand g Board
initialBoard size = loopM (clean size) size heat

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i as = (take i as) ++ ((f (as !! i) : drop (i+1) as))

count p xs = length $ filter p xs