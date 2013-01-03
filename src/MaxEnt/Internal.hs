{-# LANGUAGE TupleSections, Rank2Types #-}
module MaxEnt.Internal where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)

sumWith :: Num c => (a -> b -> c) -> [a] -> [b] -> c 
sumWith f xs = sum . zipWith f xs

pOfK :: Floating a => [a] -> [ExpectationFunction a] -> [a] -> Int -> a
pOfK values fs ls k = exp (negate . sumWith (\l f -> l * f k (values !! k)) ls $ fs) / 
    partitionFunc values fs ls 

probs :: Floating b 
      => [b] 
      -> [ExpectationFunction b] 
      -> [b] 
      -> [b]    
probs values fs ls = map (pOfK values fs ls) [0..length values - 1] 

partitionFunc :: Floating a 
              => [a] 
              -> [ExpectationFunction a]
              -> [a] 
              -> a
partitionFunc values fs ls = sum $ [ exp ((-l) * f i x) | 
                                (i, x) <- zip [0..] values, 
                                (f, l) <- zip fs ls]

objectiveFunc :: Floating a 
              => [a] 
              -> [ExpectationFunction a] 
              -> [a] 
              -> [a] 
              -> a
objectiveFunc values fs moments ls = log (partitionFunc values fs ls) 
                                   + sumWith (*) ls moments

toFunction :: (forall a. Floating a => [a] -> a) -> Function Simple
toFunction f = VFunction (f . U.toList)

toGradient :: (forall a. Floating a => [a] -> a) -> Gradient Simple
toGradient f = VGradient (U.fromList . grad f . U.toList)

toDoubleF :: (forall a. Floating a => [a] -> a) -> [Double] -> Double
toDoubleF f x = f x 

-- | Constraint type. A function and the constant it equals.
-- 
--   Think of it as the pair @(f, c)@ in the constraint 
--
-- @
--     &#931; p&#8336; f(a, x&#8336;) = c
-- @
--
--  such that we are summing over all values and @a@ is the index.
--
--  For example, for a variance constraint the @f@ would be @(\\_ x -> x*x)@ and @c@ would be the variance.
type Constraint a = (ExpectationFunction a, a)

-- | A function that takes an index and value and returns a value.
--   See 'average' and 'variance' for examples.
type ExpectationFunction a = (Int -> a -> a)

-- make a constraint from function and constant
constraint :: Floating a => ExpectationFunction a -> a -> Constraint a
constraint = (,)

-- The average constraint
average :: Floating a => a -> Constraint a
average m = constraint (const id) m

-- The variance constraint
variance :: Floating a => a -> Constraint a
variance sigma = constraint (const (^(2 :: Int))) sigma

-- | The main entry point for computing discrete maximum entropy distributions.
--   
maxent :: (forall a. Floating a => ([a], [Constraint a])) -- ^ A pair of values that the distributions is over and the constraints
       -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution 
maxent params = result where
    obj :: Floating a => [a] -> a
    obj = uncurry (objectiveFunc values) fsmoments
    
    values :: Floating a => [a]
    values = fst params
    
    constraints :: Floating a => [(ExpectationFunction a, a)]
    constraints = snd params
    
    fsmoments :: Floating a => ([ExpectationFunction a], [a])
    fsmoments = unzip constraints 
    
    fs :: [Int -> Double -> Double]
    fs = fst fsmoments
    
    -- hmm maybe there is a better way to get rid of the defaulting
    guess = U.fromList $ replicate 
        (length fs) (1.0 :: Double) 
    
    result = case unsafePerformIO (optimize defaultParameters 0.00001 guess 
                        (toFunction obj)
                        (toGradient obj)
                        Nothing) of
        (vs, ToleranceStatisfied, _) -> Right $ probs values fs (S.toList vs)
        (_, x, y) -> Left (x, y)

--test = maxent ([1.0,2.0,3.0], [average 1.5])


    
    


  
