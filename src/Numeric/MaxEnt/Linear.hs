{-# LANGUAGE TupleSections, Rank2Types #-}
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

probs :: (RealFloat b) => [[b]] -> [b] -> [b]    
probs matrix ls = map (pOfK matrix ls) [0..length matrix - 1]

pOfK :: RealFloat a => [[a]] -> [a] -> Int -> a
pOfK matrix ls k = exp (dot (transpose matrix !! k) ls) / partitionFunc matrix ls
        
-- have the column and rows backwards
partitionFunc :: RealFloat a => [[a]] -> [a] -> a
partitionFunc matrix ws = sum $ [ exp (dot as ws) | as <- transpose matrix]

-- This is almost the sam as the objectiveFunc                                   
objectiveFunc :: RealFloat a => [[a]] -> [a]-> [a] -> a
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
--
-- 
-- 
linear :: Double -- ^ Tolerance for the numerical solver
      -> (forall a. RealFloat a => LinearConstraints a) -- ^ The matrix A and column vector b
      -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution 
linear tolerance constraints = result where
   obj :: RealFloat a => [a] -> a
   obj = objectiveFunc (matrix constraints) (output constraints) 

   as :: [[Double]]
   as = matrix constraints
   
   result = probs as <$> solve tolerance (length as) obj
   
--Just for testing   
linear1 :: Double -> (forall a. RealFloat a => LinearConstraints a) -- ^ a matrix A and column vector b
     -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution 
linear1 precision constraints = result where
  obj :: RealFloat a => [a] -> a
  obj = objectiveFunc (matrix constraints) (output constraints) 

  fs :: [[Double]]
  fs = matrix constraints

  guess :: RealFloat a => [a]
  guess = replicate 
      (length fs) ((1.0) / (fromIntegral (length fs)))

  result = Right . probs fs . last $ conjugateGradientDescent obj guess
  
  

{-
    I need to clean this up and submit it
    submit the sharpen
    
    I need to figure out how to make linear2 
    what is the 
    it might help to clean up the notation for the problem
    basically I need to symbolically take the derative of the problem
    and get rid of the automatic differentation
    which I can do hand






-}









  
  





   