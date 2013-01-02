{-# LANGUAGE TupleSections, Rank2Types #-}
module MaxEnt.Internal where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)

sumWith :: Num c => (a -> b -> c) -> [a] -> [b] -> c 
sumWith f xs = sum . zipWith f xs

pOfK :: Floating a => [a] -> [a -> a] -> [a] -> Int -> a
pOfK values fs ls k = exp (negate . sumWith (\l f -> l * f (values !! k)) ls $ fs) / 
    partitionFunc values fs ls 

probs :: Floating b => [b] -> [b -> b] -> [b] -> [b]    
probs values fs ls = map (pOfK values fs ls) [0..length values - 1] 

partitionFunc :: Floating a => [a] -> [a -> a] -> [a] -> a
partitionFunc values fs ls = sum $ [ exp ((-l) * f x) | x <- values, (f, l) <- zip fs ls]

objectiveFunc :: Floating a => [a] -> [a -> a] -> [a] -> [a] -> a
objectiveFunc values fs moments ls = log (partitionFunc values fs ls) + sumWith (*) ls moments

toFunction :: (forall a. Floating a => [a] -> a) -> Function Simple
toFunction f = VFunction (f . U.toList)

toGradient :: (forall a. Floating a => [a] -> a) -> Gradient Simple
toGradient f = VGradient (U.fromList . grad f . U.toList)

toDoubleF :: (forall a. Floating a => [a] -> a) -> [Double] -> Double
toDoubleF f x = f x 

-- | Constraint type. Think of this as f and c in sum pi (f x) = c
type Constraint a = (a -> a, a)

-- make a constraint from function and constant
constraint :: Floating a => (a -> a) -> a -> Constraint a
constraint = (,)

-- The average constraint
average :: Floating a => a -> Constraint a
average m = constraint id m

-- The variance constraint
variance :: Floating a => a -> Constraint a
variance sigma = constraint (^(2 :: Int)) sigma

-- | The main entry point for computing discrete maximum entropy distributions.
--   
maxent :: (forall a. Floating a => ([a], [Constraint a])) -- ^ A pair of values that the distributions is over and the constraints
       -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution 
maxent params = result where
    obj :: Floating a => [a] -> a
    obj = uncurry (objectiveFunc values) fsmoments
    
    values :: Floating a => [a]
    values = fst params
    
    constraints :: Floating a => [(a -> a, a)]
    constraints = snd params
    
    fsmoments :: Floating a => ([a -> a], [a])
    fsmoments = unzip constraints 
    
    fs :: [Double -> Double]
    fs = fst fsmoments
    
    -- hmm maybe there is a better way to get rid of the defaulting
    guess = U.fromList $ replicate 
        (length (constraints :: [(Double -> Double, Double)])) (1.0 :: Double) 
    
    result = case unsafePerformIO (optimize defaultParameters 0.00001 guess 
                        (toFunction obj)
                        (toGradient obj)
                        Nothing) of
        (vs, ToleranceStatisfied, _) -> Right $ probs values fs (S.toList vs)
        (_, x, y) -> Left (x, y)

--test = maxent ([1.0,2.0,3.0], [average 1.5])


    
    


  
