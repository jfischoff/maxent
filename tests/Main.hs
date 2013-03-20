module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Numeric.MaxEnt
import LinearTesting
import qualified Data.Vector.Storable as S
import Data.List (intersperse)
import Numeric

un (Right x) = S.toList x

pg (Right x) = putStrLn . concat . intersperse ", " . 
                       map (($ "") . showFFloat (Just 3)) . S.toList . fst $ x
pg (Left x)  = print x

pm (Right x) = putStrLn . concat . intersperse ", " . map (($ "") . showFFloat (Just 3)) . S.toList $ x
pm (Left x)  = print x

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