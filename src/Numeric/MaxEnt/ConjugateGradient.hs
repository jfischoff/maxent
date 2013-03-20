{-# LANGUAGE TupleSections, Rank2Types #-}
module Numeric.MaxEnt.ConjugateGradient where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Data.List (transpose)
import Control.Arrow (second)


dot :: Num a => [a] -> [a] -> a
dot x y = sum . zipWith (*) x $ y

sumMap :: Num b => (a -> b) -> [a] -> b 
sumMap f = sum . map f

sumWith :: Num c => (a -> b -> c) -> [a] -> [b] -> c 
sumWith f xs = sum . zipWith f xs

minimize :: Double
      -> Int
      -> (forall s. Mode s => [AD s Double] -> AD s Double) 
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
                        (VFunction (lowerFU obj . U.toList))
                        (VGradient (U.fromList . grad obj . U.toList))
                             (Just $ VCombined (second U.fromList . grad' obj . U.toList)) of
       (vs, ToleranceStatisfied, _) -> Right vs
       (_, x, y) -> Left (x, y)