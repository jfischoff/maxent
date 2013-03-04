{-# LANGUAGE FlexibleInstances, BangPatterns #-}
module LinearTesting where
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Data.List (transpose, intersperse)
import Control.Applicative
import Numeric.MaxEnt.Linear
import qualified Data.Packed.Matrix as M
import Numeric.LinearAlgebra
import Numeric
import Debug.Trace

class Approximate a where
    (=~=) :: a -> a -> Bool

data InvalidLinearConstraints a = InvalidLinearConstraints {
        imatrix :: [[a]],
        ioutput :: [a]
    }
    deriving(Eq, Show)

instance Arbitrary (InvalidLinearConstraints Double) where
    arbitrary = sized $ \size -> do
    
        matrix <- vectorOf size (vector size)
        unnormalizedProbs <- vector size
        
        badSum <- suchThat arbitrary (/= 1.0) 
        
        let probs    = let total = sum unnormalizedProbs in 
                                map (\x -> badSum * (x/total)) unnormalizedProbs
            hmatrix     = M.fromLists matrix
            hprobs      = M.fromLists $ transpose [probs]
            inputVector = hmatrix `multiply` hprobs
            
        return . InvalidLinearConstraints matrix . (map head) . M.toLists $ inputVector

data ValidLinearConstraints a = ValidLinearConstraints {
        vmatrix :: [[a]],
        voutput :: [a]
    }
    deriving(Eq)
    
instance Approximate Double where
    x =~= y = abs (x - y) < 0.1

instance Approximate a => Approximate [a] where
    xs =~= ys = all id . zipWith (=~=) xs $ ys
    
instance Approximate (ValidLinearConstraints Double) where
    (ValidLinearConstraints mx ox) =~= (ValidLinearConstraints my oy) =
        mx =~= my && ox =~= oy

traceIt x = trace (show x) x

printRow :: [Double] -> String
printRow xs = "[" ++ 
   concat (intersperse "," (map (\x -> showFFloat (Just 6) x "") xs)) ++ "]"

instance Show (ValidLinearConstraints Double) where
   show (ValidLinearConstraints xss xs) = "matrix = [" ++ 
       concat (intersperse "," $ map printRow xss) ++ "]\n output = " ++ printRow xs

normalize xs = let total = sum xs in map (/total) xs

instance Arbitrary (ValidLinearConstraints Double) where
    arbitrary = sized $ \size' -> do
        
        let size = size' + 1
        
        matrix <- vectorOf size (vectorOf size (suchThat arbitrary (>0.0)))
        unnormalizedProbs <- vectorOf size (suchThat arbitrary (>0.0))
        
        let probs       = normalize unnormalizedProbs
            matrix'     = map normalize matrix
            hmatrix     = M.fromLists matrix'
            hprobs      = M.fromLists $ transpose [probs]
            inputVector = hmatrix `multiply` hprobs
            
        return . ValidLinearConstraints matrix' . (map head) . M.toLists $ inputVector
        

toPoly :: RealFloat a => LinearConstraints Double -> LinearConstraints a
toPoly (LinearConstraints x y) =
     LinearConstraints (map (map (fromRational . toRational)) x) 
                        (map (fromRational . toRational) y)

solvableSystemsAreSolvable :: ValidLinearConstraints Double -> Bool
solvableSystemsAreSolvable (ValidLinearConstraints x y) = 
    case linear 0.0000005 (toPoly $ LinearConstraints x y) of
        Right _ -> True
        Left  _ -> False
      
traceItNote msg x = trace (msg ++ " " ++ show x) x

probsSumToOne :: ValidLinearConstraints Double -> Bool
probsSumToOne (ValidLinearConstraints x y) = 
    case linear 0.000005 (toPoly $ LinearConstraints x y) of
        Right ps -> case 1.0 =~= sum ps of
            True -> True
            False -> trace ("new probs" ++ show ps) False
        Left _   -> False
        
solutionFitsConstraints :: ValidLinearConstraints Double -> Bool
solutionFitsConstraints (ValidLinearConstraints x y) = 
    case linear 0.000005 (toPoly $ LinearConstraints x y) of
        Right ps -> result where
            result = ((map head) . M.toLists $ inputVector) =~= y
    
            hmatrix     = M.fromLists x
            hprobs      = M.fromLists $ transpose [traceItNote "ps" ps]
            inputVector = traceItNote "inputVector" $ hmatrix `multiply` hprobs
            
            
        Left _   -> False

--This is not the test I want ..  but it does seem to work
-- TODO you want to test against the original probs which should 
-- have less then or equal to the estimated
entropyIsGreaterOrEqual :: ValidLinearConstraints Double -> Bool
entropyIsGreaterOrEqual (ValidLinearConstraints x y) = 
    case linear 0.000005 (toPoly $ LinearConstraints x y) of
        Right ps -> result where
            entropy xs = negate . sum . map (\x -> x * log x) $ xs
            
            yEntropy         = entropy y
            estimatedEntropy = entropy ps
            
            result = yEntropy >= estimatedEntropy
        Left  _  -> error "failed!"


--probabilityNeighborhood ps = 

-- TODO make this
entropyIsMaximum :: ValidLinearConstraints Double -> Bool
entropyIsMaximum (ValidLinearConstraints x y) = 
    case linear 0.000005 (toPoly $ LinearConstraints x y) of
        Right ps -> result where
            entropy xs = negate . sum . map (\x -> x * log x) $ xs
            
            yEntropy         = entropy y
            estimatedEntropy = entropy ps
            
            result = yEntropy >= estimatedEntropy
        Left  _  -> error "failed!"


-- also if it is a maximum a small change in either direction that still fits the constraints
-- should not lower the entropy
-- 


--main = quickCheck probsSumToOne














