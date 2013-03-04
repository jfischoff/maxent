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

data LinearConstraints a = LinearConstraints {
        matrix :: [[a]], 
        output :: [a]
    }
    deriving (Show, Eq)

-- | This is for the linear case Ax = b 
--   @x@ in this situation is the vector of probablities.
--  
--  For example.
-- 
-- @
--   maxentLinear ([[0.85, 0.1, 0.05], [0.25, 0.5, 0.25], [0.05, 0.1, 0.85]], [0.29, 0.42, 0.29])
-- @
--
-- Right [0.1, 0.8, 0.1]
-- 
-- To be honest I am not sure why I can't use the 'maxent' version to solve
-- this type of problem, but it doesn't work. I'm still learning
-- 
linear :: Double -> (forall a. RealFloat a => LinearConstraints a) -- ^ a matrix A and column vector b
      -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution 
linear precision constraints = result where
   obj :: RealFloat a => [a] -> a
   obj = objectiveFunc (matrix constraints) (output constraints) 
   --obj = squaredGrad $ objectiveFunc (matrix constraints) (output constraints) 

   as :: [[Double]]
   as = matrix constraints
   
   result = probs as <$> solve precision (length as) obj
   
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









  
  





   