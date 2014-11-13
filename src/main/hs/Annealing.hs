{-# LANGUAGE ScopedTypeVariables #-}

module Annealing where

import Control.Monad ((>>=), sequence)
import Control.Monad.Random (Rand, Random, RandomGen, getRandomR)
import Data.List (unfoldr, find)

class Annealable a where
    heat :: (RandomGen g) => a -> Rand g a
    energy :: a -> Double

newtype Temperature = Temperature Double deriving (Eq, Show)

loopM :: (Monad m) => a -> Int -> (a -> m a) -> m a
loopM a 0 _ = return a
loopM a n f = (loopM a (n-1) f) >>= f 

anneal :: (RandomGen g, Annealable a) => a -> [Temperature] -> Int -> Rand g (Maybe a)
anneal start temperatures stepsAtEachTemperature = fmap (\x -> find (\a -> energy a == 0) x) trialSolutions
    where trialSolutions = listTrialSolutions start temperatures stepsAtEachTemperature

temperatures :: Temperature -> Temperature -> Double -> [Temperature]
temperatures it@(Temperature initialTemperature) (Temperature finalTemperature) temperatureDropRatio = temperatures
    where step t@(Temperature tv) = if tv > finalTemperature then Just $ (t, Temperature $ tv * temperatureDropRatio) else Nothing
          temperatures = unfoldr step it

listTrialSolutions :: (RandomGen g, Annealable a) => a -> [Temperature] -> Int -> Rand g [a]
listTrialSolutions start temperatures stepsAtEachTemperature = 
    let choose' temperature trialSolution current threshold = choose trialSolution current threshold (acceptanceProbability temperature) energy in 
    sequence $ map (\temperature -> loopM start stepsAtEachTemperature (testTrialSolution $ choose' temperature)) temperatures

testTrialSolution :: (Annealable a, RandomGen g) => (a -> a -> Double -> a) -> a -> Rand g a
testTrialSolution choose current = do
    trialSolution <- heat current
    threshold <- getRandomR ((0.0 :: Double), 1.0)
    return $ choose trialSolution current threshold

acceptanceProbability :: Temperature -> (Double -> Double)
acceptanceProbability (Temperature t) d = exp ((-d) / t)

choose :: a -> a -> Double -> (Double -> Double) -> (a -> Double) -> a
choose trialSolution current threshold acceptanceProbability energy = 
    let improvement = (energy trialSolution) - (energy current) in
    if improvement < 0 then
        trialSolution
    else
        let calc = acceptanceProbability improvement in
        if calc > threshold then trialSolution else current

