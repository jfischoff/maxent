{-# LANGUAGE FlexibleInstances, BangPatterns, Rank2Types, StandaloneDeriving #-}

module LinearTesting 
    ( properties
    ) where

import Control.Applicative

import Data.List (intersperse)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Packed.Matrix as M
import Data.Traversable

import Debug.Trace

import Foreign.Storable

import Numeric (showFFloat)
import Numeric.AD
import Numeric.MaxEnt.Linear
import Numeric.LinearAlgebra

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------
-- Test groups
--------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty "solvableSystemsAreSolvable" solvableSystemsAreSolvable
    , testProperty "probsSumToOne" probsSumToOne
    , testProperty "solutionFitsConstraints" solutionFitsConstraints   
    ] 

--------------------------------------------------------------------------------
-- QuickCheck and helper datatypes, instances, etc.
--------------------------------------------------------------------------------

class Approximate a where
    (=~=) :: a -> a -> Bool

data InvalidLinearConstraints a = InvalidLinearConstraints
  { imatrix :: V.Vector (V.Vector a)
  , ioutput :: V.Vector a
  } deriving (Eq, Show)

instance Arbitrary (InvalidLinearConstraints Double) where
    arbitrary = sized $ \size -> do
    
        matrix <- vectorOf size $ vector size
        unnormalizedProbs <- vector size
        
        badSum <- suchThat arbitrary (/= 1.0) 
        
        let probs    = let total = sum unnormalizedProbs in 
                                map (\x -> badSum * (x/total)) unnormalizedProbs
            probs' = V.fromList probs
            matrix' = V.fromList $ map V.fromList matrix
            inputVector = matrix' `multMV` probs'
            
        return $ InvalidLinearConstraints matrix' inputVector

data ValidLinearConstraints = VLC
  { unVLC :: forall a. Floating a => (V.Vector (V.Vector a), V.Vector a) }

toLC :: ValidLinearConstraints -> LinearConstraints
toLC vlc = LC $ unVLC vlc

deriving instance Eq ValidLinearConstraints
    
instance Approximate Double where
    x =~= y = abs (x - y) < 0.1

instance Approximate a => Approximate [a] where
    xs =~= ys = all id . zipWith (=~=) xs $ ys

instance Approximate a => Approximate (V.Vector a) where
    xs =~= ys = xs' =~= ys' where
        xs' = V.toList xs
        ys' = V.toList ys
    
instance (Approximate a, Storable a) => Approximate (S.Vector a) where
    xs =~= ys = S.all id . S.zipWith (=~=) xs $ ys
    
instance Approximate ValidLinearConstraints where
    (VLC (mx, ox)) =~= (VLC (my, oy)) =
        (mx :: V.Vector (V.Vector Double)) =~= my && (ox :: V.Vector Double) =~= oy

traceIt x = trace (show x) x

printRow :: [Double] -> String
printRow xs = "[" ++ 
   concat (intersperse "," (map (\x -> showFFloat (Just 6) x "") xs)) ++ "]"

instance Show ValidLinearConstraints where
   show (VLC (xss, xs)) = "matrix = [" ++ 
       concat (intersperse "," $ map printRow xss') ++ "]\n output = " ++ printRow xs' where
        xss' = map V.toList $ V.toList xss
        xs' = V.toList xs

normalize xs = let total = V.sum xs in V.map (/total) xs

instance Arbitrary ValidLinearConstraints where
    arbitrary = sized $ \size' -> do
        
        let size = size' + 1
        
        matrixList <- vectorOf size $ vectorOf size $ suchThat arbitrary (>0.0) :: Gen [[Double]]
        unnormalizedProbsList <- vectorOf size $ suchThat arbitrary (>0.0) :: Gen [Double]
        
        let probs       = normalize $ V.fromList unnormalizedProbsList
            matrix'     = V.map normalize $ V.fromList $ map V.fromList matrixList
            output = matrix' `multMV` probs
            
        return $ VLC ( V.map (V.map (fromRational . toRational)) matrix'
                     , V.map (fromRational . toRational) output)
        
--
--toPoly :: RealFloat a => LinearConstraints Double -> LinearConstraints a
--toPoly (LC x y) =
--     LC (map (map (fromRational . toRational)) x) 
--                        (map (fromRational . toRational) y)

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

solvableSystemsAreSolvable :: ValidLinearConstraints -> Bool
solvableSystemsAreSolvable vlc =
    case linear (1e-3) (toLC vlc) of
        Right _ -> True
        Left  _ -> False
      
traceItNote msg x = trace (msg ++ " " ++ show x) x

probsSumToOne :: ValidLinearConstraints -> Bool
probsSumToOne vlc =
    case linear (1e-3) (toLC vlc) of
        Right ps -> case 1.0 =~= S.sum ps of
            True -> True
            False -> trace ("new probs" ++ show ps) False
        Left _   -> False
        
solutionFitsConstraints :: ValidLinearConstraints -> Bool
solutionFitsConstraints vlc = let lc = toLC vlc in
    case linear (1e-3) lc of
        Right ps -> result where
            (x, y) = unLC lc
            result = inputVector =~= y
    
            ps' = V.fromList $ S.toList ps
            inputVector = x `multMV` ps'
        Left _   -> False

--------------------------------------------------------------------------------
-- Unfinished stuff. After finishing, remember to move to the appropriate place
-- and add to an exported test group.
--------------------------------------------------------------------------------

--This is not the test I want ..  but it does seem to work
-- TODO you want to test against the original probs which should 
-- have less then or equal to the estimated
entropyIsGreaterOrEqual :: ValidLinearConstraints -> Bool
entropyIsGreaterOrEqual vlc = let lc = toLC vlc in
    case linear 0.000005 lc of
        Right ps -> result where
            entropy xs = negate . sum . map (\x -> x * log x) $ xs
            
            yEntropy         = entropy . V.toList . snd $ unLC lc
            estimatedEntropy = entropy $ S.toList ps
            
            result = yEntropy >= estimatedEntropy
        Left  _  -> error "failed!"


--probabilityNeighborhood ps = 

-- TODO make this
entropyIsMaximum :: ValidLinearConstraints -> Bool
entropyIsMaximum vlc = let lc = toLC vlc in
    case linear 0.000005 lc of
        Right ps -> result where
            entropy xs = negate . sum . map (\x -> x * log x) $ xs
            
            yEntropy         = entropy . V.toList . snd $ unLC lc
            estimatedEntropy = entropy $ S.toList ps
            
            result = yEntropy >= estimatedEntropy
        Left  _  -> error "failed!"


-- also if it is a maximum a small change in either direction that still fits the constraints
-- should not lower the entropy
-- 
