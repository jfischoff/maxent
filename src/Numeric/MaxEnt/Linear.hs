{-# LANGUAGE TupleSections, Rank2Types, NoMonomorphismRestriction #-}
module Numeric.MaxEnt.Linear where
import Numeric.MaxEnt.ConjugateGradient
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Data.List (transpose)
import Control.Applicative
import qualified Data.Vector.Storable as S
  
probs matrix ls = map (pOfK matrix ls) [0..length matrix - 1]

pOfK matrix ls k = exp (dot (transpose matrix !! k) ls) / partitionFunc matrix ls
        
-- have the column and rows backwards

partitionFunc matrix ws = sum $ [ exp (dot as ws) | as <- transpose matrix]

-- This is almost the sam as the objectiveFunc                                   

objectiveFunc as moments ls = log (partitionFunc as ls) - dot ls moments

data LinearConstraints a = LC {
        matrix :: [[a]], 
        output :: [a]
    }
    deriving (Show, Eq)

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
linear :: Double 
      -- ^ Tolerance for the numerical solver
      -> LinearConstraints Double
      -- ^ The matrix A and column vector b
      -> Either (Result, Statistics) (S.Vector Double)
      -- ^ Either the a discription of what wrong or the probability distribution 
linear tolerance constraints = result where
   obj = objectiveFunc (map (map auto) $ matrix constraints) (map auto $ output constraints) 

   as = matrix constraints
   
   result = (S.fromList . probs as . S.toList) <$> minimize tolerance count obj
   
   count = length $ output constraints 
   
   guess = U.fromList $ replicate 
         count ((1.0 :: Double) / (fromIntegral count))
    
   

  
  












  
  





   