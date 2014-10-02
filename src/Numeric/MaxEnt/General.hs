{-# LANGUAGE TupleSections, Rank2Types #-}

module Numeric.MaxEnt.General (
    Constraint,
    general
 ) where

import Control.Applicative

import qualified Data.Vector.Storable as S

import Numeric.Optimization.Algorithms.HagerZhang05 (Result, Statistics)
import Numeric.AD.Lagrangian

entropy :: Floating a => [a] -> a
entropy xs = negate . sum . map (\x -> x * log x) $ xs

-- | A more general solver. This directly solves the lagrangian of the constraints and the
--  the additional constraint that the probabilities must sum to one.
general :: Double 
        -- ^ Tolerance for the numerical solver
        -> Int
        -- ^ the count of probabilities
        -> [Constraint]
        -- ^  constraints
        -> Either (Result, Statistics) (S.Vector Double) 
        -- ^ Either the a discription of what wrong or the probability distribution
general tolerance count constraints = 
    fst <$> maximize entropy ((sum <=> 1) : constraints) tolerance count
