module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Numeric.MaxEnt
import LinearTesting

-- One test is that a list convolved with a 
-- guassian and then infered with a gaussian should have less entropy then it
-- started with

main = defaultMain [
        testGroup "Linear Tests" [
            testProperty "solvableSystemsAreSolvable" solvableSystemsAreSolvable,
            testProperty "probsSumToOne" probsSumToOne,
            testProperty "solutionFitsConstraints" solutionFitsConstraints   
        ]
    ] 