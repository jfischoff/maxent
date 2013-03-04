{-# LANGUAGE TupleSections, Rank2Types #-}
module Numeric.MaxEnt.Internal where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Data.List (transpose)
--import Numeric.AD.Lagrangian
import Numeric.MaxEnt.ConjugateGradient

pOfK :: RealFloat a => [a] -> [ExpectationFunction a] -> [a] -> Int -> a
pOfK values fs ls k = exp (negate . sumWith (\l f -> l * f (values !! k)) ls $ fs) / 
    partitionFunc values fs ls 



probs :: (RealFloat b)
      => [b] 
      -> [ExpectationFunction b] 
      -> [b] 
      -> [b]    
probs values fs ls = map (pOfK values fs ls) [0..length values - 1] 

partitionFunc :: RealFloat a
              => [a] 
              -> [ExpectationFunction a]
              -> [a] 
              -> a
partitionFunc values fs ls = sum $ [ exp ((-l) * f x) | 
                                x <- values, 
                                (f, l) <- zip fs ls]

objectiveFunc :: RealFloat a
              => [a] 
              -> [ExpectationFunction a] 
              -> [a] 
              -> [a] 
              -> a
objectiveFunc values fs moments ls = log (partitionFunc values fs ls) 
                                   + sumWith (*) ls moments








entropy :: RealFloat a => [a] -> a
entropy = negate . sumMap (\p -> p * log p) 


-- This a functions that takes in a list of values and 
-- a list of probabilities
type GeneralConstraint a = [a] -> [a] -> a


lagrangian :: RealFloat a
             => ([a], [GeneralConstraint a], [a]) 
             -> [a] 
             -> a
lagrangian (values, fs, constants) lamsAndProbs = result where
    result = entropy ps + (sum $ zipWith (*) lams constraints)
    constraints        = zipWith (-) appliedConstraints constants
    appliedConstraints = map (\f -> f values ps) fs
        
    -- split the args list
    ps   = take (length values) lamsAndProbs
    lams = drop (length values) lamsAndProbs



generalObjectiveFunc :: RealFloat a => (forall b. RealFloat b => 
             ([b], [GeneralConstraint b], [b]))
             -> [a] 
             -> a
generalObjectiveFunc params lamsAndProbs = 
    squaredGrad lang lamsAndProbs  where
        
    lang :: RealFloat a => (forall s. Mode s => [AD s a] -> AD s a)
    lang = lagrangian params




-- make a constraint from function and constant
constraint :: RealFloat a => ExpectationFunction a -> a -> Constraint a
constraint = (,)

-- The average constraint
average :: RealFloat a => a -> Constraint a
average m = constraint id m

-- The variance constraint
variance :: RealFloat a => a -> Constraint a
variance sigma = constraint (^(2 :: Int)) sigma

-- | Most general solver
--   This will solve the langrangian by added the constraint that the 
--   probabilities must add up to zero.
--   This is the slowest but most flexible method. 

generalMaxent :: (forall a. RealFloat a => ([a], [(GeneralConstraint a, a)])) -- ^ A pair of values that the distributions is over and the constraints
       -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution
generalMaxent params = result where
   obj :: RealFloat a => [a] -> a
   obj = generalObjectiveFunc objInput

   values :: RealFloat a => [a]
   values = fst params

   constraints :: RealFloat a => [(GeneralConstraint a, a)]
   constraints = snd params

   fsmoments :: RealFloat a => ([GeneralConstraint a], [a])
   fsmoments = unzip constraints 
   
   --The new constraint is always needed for probability problems
   
   sumToOne :: RealFloat a => GeneralConstraint a
   sumToOne _ xs = sumMap id xs
   
   gcons' :: RealFloat a => [GeneralConstraint a]
   gcons' = fst fsmoments
   
   gcons :: RealFloat a => [GeneralConstraint a]
   gcons = sumToOne : gcons'
   
   moments :: RealFloat a => [a]
   moments = 1 : snd fsmoments
   
   
   objInput :: RealFloat b => ([b], [GeneralConstraint b], [b])
   objInput = (values, gcons, moments)

   fs :: [[Double] -> [Double] -> Double]
   fs = fst fsmoments

   --hmm maybe there is a better way to get rid of the defaulting
   guess = U.fromList $ replicate 
       (length fs) (1.0 :: Double) 

   result = case unsafePerformIO (optimize defaultParameters 0.00001 guess 
                        (toFunction obj)
                        (toGradient obj)
                       Nothing) of
       -- Not sure what is supposed to happen here
       -- I guess I can plug the lambdas back in
       (vs, ToleranceStatisfied, _) -> Right $  (S.toList vs)
       (_, x, y) -> Left (x, y)
       
       
-- | Constraint type. A function and the constant it equals.
-- 
--   Think of it as the pair @(f, c)@ in the constraint 
--
-- @
--     &#931; p&#8336; f(x&#8336;) = c
-- @
--
--  such that we are summing over all values .
--
--  For example, for a variance constraint the @f@ would be @(\\x -> x*x)@ and @c@ would be the variance.
type Constraint a = (ExpectationFunction a, a)

-- | A function that takes an index and value and returns a value.
--   See 'average' and 'variance' for examples.
type ExpectationFunction a = (a -> a)

-- | The main entry point for computing discrete maximum entropy distributions.
--   Where the constraints are all moment constraints. 
maxent :: (forall a. RealFloat a => ([a], [Constraint a])) -- ^ A pair of values that the distributions is over and the constraints
       -> Either (Result, Statistics) [Double] -- ^ Either the a discription of what wrong or the probability distribution 
maxent params = result where
    obj :: RealFloat a => [a] -> a
    obj = uncurry (objectiveFunc values) fsmoments
    
    values :: RealFloat a => [a]
    values = fst params
    
    constraints :: RealFloat a => [(ExpectationFunction a, a)]
    constraints = snd params
    
    fsmoments :: RealFloat a => ([ExpectationFunction a], [a])
    fsmoments = unzip constraints 
    
    fs :: [Double -> Double]
    fs = fst fsmoments
    
    -- hmm maybe there is a better way to get rid of the defaulting
    guess = U.fromList $ replicate 
        (length fs) (1.0 :: Double) 
    
    {-
    result = case unsafePerformIO (optimize (defaultParameters { printFinal = False }) 
                        0.001 guess 
                        (toFunction obj)
                        (toGradient obj)
                        Nothing) of
        (vs, ToleranceStatisfied, _) -> Right $ probs values fs (S.toList vs)
        (_, x, y) -> Left (x, y)
    -}
    
    result = case unsafePerformIO (optimize (defaultParameters { printFinal = False }) 
                        0.001 guess 
                        (toFunction obj)
                        (toGradient obj)
                        Nothing) of
        (vs, ToleranceStatisfied, _) -> Right $ probs values fs (S.toList vs)
        (_, x, y) -> Left (x, y)



    
    


  
