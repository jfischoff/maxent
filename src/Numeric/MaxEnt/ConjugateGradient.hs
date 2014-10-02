{-# LANGUAGE Rank2Types #-}

--------------------------------------------------------------------------------
-- This module is updated to work with version 4.* of `Numeric.AD`, but it is
-- now provides funcationality only to `Numeric.MaxEnt.Linear`. Formerly,
-- `Numeric.MaxEnt.Moment` used the `minimize` function defined here, but I
-- rewrote that module to use the `general` function defined in
-- `Numeric.MaxEnt.General`, which in turn uses `maximize` from the
-- `Numeric.AD.Lagrangian`.
--
-- I intend to rewrite `Numeric.MaxEnt.Linear` so that it no longer relies on
-- this module either, with would leave it unused.  -- E.P.
--------------------------------------------------------------------------------

module Numeric.MaxEnt.ConjugateGradient where

import Control.Arrow (second)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S

import GHC.IO (unsafePerformIO)

import Numeric.Optimization.Algorithms.HagerZhang05
import Numeric.AD

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum $ zipWith (*) xs ys

sumMap :: Num b => (a -> b) -> [a] -> b 
sumMap f = sum . map f

sumWith :: Num c => (a -> b -> c) -> [a] -> [b] -> c 
sumWith f xs ys = sum $ zipWith f xs ys

minimize :: Double
      -> Int
      -> (forall a. (Floating a) => [a] -> a) 
      -> Either (Result, Statistics) (S.Vector Double)
minimize tolerance count obj = result where
      guess = U.fromList $ 1 : replicate (count - 1) 0
     
      result = case unsafePerformIO $ 
                    optimize 
                        (defaultParameters { printFinal = False,
                                             --printParams = True,
                                             --maxit = 10000,
                                             --verbosity = VeryVerbose, 
                                             initialStep = Just 0.1
                                             --lineSearch = ApproximateWolfe,
                                             --debugTol = Just 0.01,
                                             --nanRho = 1.3,
                                             --estimateError = RelativeEpsilon 0.1 
                                             --lbfgs = False 
                                             }) 
                        tolerance 
                        guess 
                        (VFunction (obj . U.toList))
                        (VGradient (U.fromList . grad obj . U.toList))
                             (Just $ VCombined (second U.fromList . grad' obj . U.toList)) of
       (vs, ToleranceStatisfied, _) -> Right vs
       (_, x, y) -> Left (x, y)
