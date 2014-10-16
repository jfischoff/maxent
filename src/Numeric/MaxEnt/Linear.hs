{-# LANGUAGE FlexibleContexts, Rank2Types, NoMonomorphismRestriction,
             StandaloneDeriving #-}

module Numeric.MaxEnt.Linear where

import Control.Applicative

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

import Numeric.MaxEnt.ConjugateGradient (minimize)
import Numeric.Optimization.Algorithms.HagerZhang05 (Result, Statistics)
import Numeric.AD

dot :: Num a => V.Vector a -> V.Vector a -> a
dot xs ys = V.sum $ V.zipWith (*) xs ys

transpose :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transpose vec
  | V.null vec = vec
  | V.null $ V.head vec = transpose $ V.tail vec
  | otherwise =
        (x `V.cons` (heads xss)) `V.cons` (transpose $ xs `V.cons` (tails xss))
      where
        x = V.head $ V.head vec
        xs = V.tail $ V.head vec
        xss = V.tail vec

heads :: V.Vector (V.Vector a) -> V.Vector a
heads ys | V.null ys = V.empty
         | otherwise = V.map V.head ys -- Not safe

tails :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
tails ys | V.null ys = V.empty
         | otherwise = V.map V.tail ys

multMV :: (Num a) => V.Vector (V.Vector a) -> V.Vector a -> V.Vector a
multMV mat vec = V.map (\row -> dot row vec) mat
  
probs :: (Floating a) => V.Vector (V.Vector a) -> V.Vector a -> V.Vector a
probs matrix ls = result where
    norm = partitionFunc matrix ls
    result = V.map (\x -> exp x / norm ) $ (transpose matrix) `multMV` ls

partitionFunc :: (Floating a) => V.Vector (V.Vector a) -> V.Vector a -> a
partitionFunc matrix ws = V.sum . V.map exp . multMV (transpose matrix) $ ws

objectiveFunc :: (Floating a)
              => V.Vector (V.Vector a) -> V.Vector a -> V.Vector a -> a
objectiveFunc as moments ls = log $ partitionFunc as ls - dot ls moments

data LinearConstraints = LC
  { unLC :: forall a. (Floating a) => (V.Vector (V.Vector a), V.Vector a) }

-- These instances default the underlying numeric type of `LC` to `Double`,
-- which may be problematic for some usages.
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
--   >>> linear 3.0e-17 $ LC ([[0.68, 0.22, 0.1], [0.1, 0.68, 0.22], [0.22, 0.1, 0.68]], [0.276, 0.426, 0.298])
--   Right (fromList [0.2000000000000001,0.49999999999999983,0.30000000000000004])
--
--   I fell compelled to point out that we could also just invert the original
--   convolution matrix. Supposedly using maxent can reduce errors from noise if
--   the convolution matrix is not properly estimated.
linear :: Double 
       -- ^ Tolerance for the numerical solver
       -> LinearConstraints
       -- ^ The matrix A and column vector b
       -> Either (Result, Statistics) (S.Vector Double)
       -- ^ Either a description of what went wrong or the probability
       --   distribution 
linear tolerance constraints  =
    let (matrix, output) = unLC constraints
        obj = objectiveFunc matrix output . V.fromList
        n = V.length output
        s2v = V.fromList . S.toList
        v2s = S.fromList . V.toList
    in v2s . probs matrix . s2v <$> minimize tolerance n obj



--------------------------------------------------------------------------------
-- I updated everything below to work with the new types, but it's not clear to 
-- me what it's for.  -- EP
--------------------------------------------------------------------------------

linear' :: (Floating a, Ord a)
        => LinearConstraints
        -- ^ The matrix A and column vector b
        -> V.Vector (V.Vector a)
        -- ^ Either a description of what went wrong or the probability
        --   distribution
linear' constraints =
    let (matrix, output) = unLC constraints
        obj = objectiveFunc matrix output
        guess = 1 `V.cons` V.replicate (V.length output - 1) 0
    in V.map (probs matrix) . V.fromList . gradientDescent obj $ guess
    
linear'' :: (Floating a, Ord a)
         => LinearConstraints
         -- ^ The matrix A and column vector b
         -> V.Vector (V.Vector a)
         -- ^ Either a description of what went wrong or the probability
         --   distribution
linear'' constraints =
    let (matrix, output) = unLC constraints
        obj = objectiveFunc matrix output 
        guess = 1 `V.cons` V.replicate (V.length output - 1) 0
    in V.map (probs matrix) . V.fromList . conjugateGradientDescent obj $ guess

--test1 = LC ( [ [0.892532,0.003851,0.063870,0.001593,0.038155]
--             , [0.237713,0.111149,0.326964,0.271535,0.052639]
--             , [0.133708,0.788233,0.051543,0.003976,0.022539]
--             , [0.238064,0.263171,0.112279,0.270452,0.116034]
--             , [0.844155,0.011312,0.001470,0.001826,0.141237]
--             ]
--           ,
--             [0.246323,0.235600,0.071699,0.211339,0.238439]
--           )
