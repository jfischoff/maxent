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


dot :: Num a => [a] -> [a] -> a
dot x y = sum . zipWith (*) x $ y

sumMap :: Num b => (a -> b) -> [a] -> b 
sumMap f = sum . map f

sumWith :: Num c => (a -> b -> c) -> [a] -> [b] -> c 
sumWith f xs = sum . zipWith f xs


toFunction :: (forall a. RealFloat a => [a] -> a) -> Function Simple
toFunction f = VFunction (f . U.toList)

toGradient :: (forall a. RealFloat a => [a] -> a) -> Gradient Simple
toGradient f = VGradient (U.fromList . grad f . U.toList)

toDoubleF :: (forall a. RealFloat a => [a] -> a) -> [Double] -> Double
toDoubleF f x = f x

squaredGrad :: Num a 
            => (forall s. Mode s => [AD s a] -> AD s a) -> [a] -> a
squaredGrad f vs = sumMap (\x -> x*x) (grad f vs)

solve :: Double
      -> Int
      -> (forall a. RealFloat a => [a] -> a) 
      -> Either (Result, Statistics) [Double]
solve percision count obj = result where
      guess = U.fromList $ replicate 
          count ((1.0 :: Double) / (fromIntegral count))
     
      result = case unsafePerformIO (optimize defaultParameters percision guess 
                             (toFunction obj)
                             (toGradient obj)
                             Nothing) of
       (vs, ToleranceStatisfied, _) -> Right . S.toList $ vs
       (_, x, y) -> Left (x, y)