{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Annealing
import Data.Functor (fmap)
import Test.Hspec
import System.Random
import Control.Monad.Random (Rand, evalRand)

newtype TestAnnealable = TestAnnealable String deriving (Show, Eq)

instance Annealable TestAnnealable where
    heat (TestAnnealable a) = return $ TestAnnealable $ 'h' : a
    energy (TestAnnealable a) = fromIntegral $ length a

newtype TestDouble = TestDouble Double deriving (Show, Eq)

instance Random TestDouble where
    randomR _ g = (0.5, g)
    random = undefined

main :: IO()
main = hspec $ do
    describe "temperatures" $ do
        it "is a list of declining temperatures" $ do
            let ts = temperatures (Temperature 100.0) (Temperature 3.26) 0.5
            ts `shouldBe` fmap Temperature [100.0, 50.0, 25.0, 12.5, 6.25]

    describe "choose" $ 
        let worse = "TT"
            better = "C"
            energy' = fromIntegral . length
        in do
        it "chooses the trial solution if it is better" $ do
            choose better worse 0.5 (const 0.5) energy' `shouldBe` better

        it "chooses the trial solution if it the acceptance probability allows it" $ do
            choose worse better 0.4 (const 0.5) energy' `shouldBe` worse

        it "chooses the current solution if the acceptance probability doesn't allowthe acceptance probability allows it it" $ do
            choose worse better 0.6 (const 0.5) energy' `shouldBe` better

    describe "testTrialSolution" $ do
        it "heats the current, chooses a threshold, and calls choose" $ do
            let current = TestAnnealable "T"
            let chosen = TestAnnealable "C"
            let choose (TestAnnealable "hT") current 5.4137706025506604e-3 = chosen
            let actual = testTrialSolution choose current :: Rand MockRandomGen TestAnnealable 
            evalRand actual (MockRandomGen 0) `shouldBe` chosen

