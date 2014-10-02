{-# LANGUAGE ExistentialQuantification, Rank2Types, TupleSections,
             NoMonomorphismRestriction, StandaloneDeriving #-}

module Numeric.MaxEnt.Linear where

import Data.List (transpose)
import qualified Data.Vector.Storable as S

import Control.Applicative

import Numeric.MaxEnt.ConjugateGradient (minimize, dot)
import Numeric.AD
  
multMV mat vec = map (\row -> dot row vec) mat
  
probs :: (Floating a) => [[a]] -> [a] -> [a]
probs matrix ls = result where
    norm = partitionFunc matrix ls
    result = map (\x -> exp x / norm ) $ (transpose matrix) `multMV` ls

partitionFunc matrix ws = sum . map exp . multMV (transpose matrix) $ ws

-- This is almost the sam as the objectiveFunc                                   

objectiveFunc :: (Floating a) => [[a]] -> [a] -> [a] -> a
objectiveFunc as moments ls = log $ partitionFunc as ls - dot ls moments

data LinearConstraints = LC
  { unLC :: forall a. (Floating a) => ([[a]], [a]) }

deriving instance Eq LinearConstraints
deriving instance Show LinearConstraints

-- | This is for the linear case Ax = b 
--   @x@ in this situation is the vector of probablities.
--  
--  Consider the 1 dimensional circular convolution using hmatrix.
--  
--  >>> import Numeric.LinearAlgebra
--  >>> fromLists [[0.68, 0.22, 0.1], [0.1, 0.68, 0.22], [0.22, 0.1, 0.68]] <> fromLists [[0.2], [0.5], [0.3]]
--  (3><1) [0.276, 0.426, 0.298]   
-- 
--   Now if we were given just the convolution and the output, we can use 'linear' to infer the input.
-- 
--   >>> linear 3.0e-17 $ LC [[0.68, 0.22, 0.1], [0.1, 0.68, 0.22], [0.22, 0.1, 0.68]] [0.276, 0.426, 0.298]
--   Right [0.20000000000000004,0.4999999999999999,0.3]
--   
--   I fell compelled to point out that we could also just invert the original convolution 
--   matrix. Supposedly using maxent can reduce errors from noise if the convolution 
--   matrix is not properly estimated.
-- 
--linear :: Double 
--       -- ^ Tolerance for the numerical solver
--       -> LinearConstraints
--       -- ^ The matrix A and column vector b
--       -> Either (Result, Statistics) (S.Vector Double)
--       -- ^ Either the a discription of what wrong or the probability distribution 
linear tolerance constraints  = result where
    (matrix, output) = unLC constraints
    obj = objectiveFunc matrix output 

    
    matrix'    = matrix
    count = length $ output 

    result = (S.fromList . probs matrix' . S.toList) <$> minimize tolerance count obj
   

--linear' :: LinearConstraints
--         -- ^ The matrix A and column vector b
--         -> [[Double]]
--         -- ^ Either the a discription of what wrong or the probability distribution
linear' constraints = result where
    (matrix, output) = unLC constraints
    obj = objectiveFunc (matrix) (output) 

    matrix' = matrix
    count = length $ output
    guess = 1 : replicate (count - 1) 0

    result = map (probs matrix') . gradientDescent obj $ guess
    
    
--linear'' :: LinearConstraints
--         -- ^ The matrix A and column vector b
--         -> [[Double]]
--         -- ^ Either the a discription of what wrong or the probability distribution
linear'' constraints = result where
    (matrix, output) = unLC constraints
    obj = objectiveFunc matrix output 

    matrix' = matrix
    count = length $ output
    guess = 1 : replicate (count - 1) 0

    result = map (probs matrix') . conjugateGradientDescent obj $ guess

--test1 = LC ( [ [0.892532,0.003851,0.063870,0.001593,0.038155]
--             , [0.237713,0.111149,0.326964,0.271535,0.052639]
--             , [0.133708,0.788233,0.051543,0.003976,0.022539]
--             , [0.238064,0.263171,0.112279,0.270452,0.116034]
--             , [0.844155,0.011312,0.001470,0.001826,0.141237]
--             ]
--           ,
--             [0.246323,0.235600,0.071699,0.211339,0.238439]
--           )
