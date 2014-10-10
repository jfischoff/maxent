module Main where

import Data.List (intersperse)
import qualified Data.Vector.Storable as S

import Numeric (showFFloat)

import Test.Tasty

import LinearTesting as Linear


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Linear.properties]



--------------------------------------------------------------------------------
-- Cross-module tests.
--------------------------------------------------------------------------------

-- TODO: Write tests that compare results using different implementationds,
-- e.g., using `Linear.linear` and `Moment.maxent` for problems with linear
-- moment constraints.

--------------------------------------------------------------------------------
-- Unused stuff. Keep around or delete?
--------------------------------------------------------------------------------

un (Right x) = S.toList x

pg (Right x) = putStrLn . concat . intersperse ", " . 
                       map (($ "") . showFFloat (Just 3)) . S.toList . fst $ x
pg (Left x)  = print x

pm (Right x) = putStrLn . concat . intersperse ", " . map (($ "") . showFFloat (Just 3)) . S.toList $ x
pm (Left x)  = print x


-- One test is that a list convolved with a 
-- guassian and then infered with a gaussian should have less entropy then it
-- started with
