module Excersise4 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

----------------------------

isPermutation :: Eq a => [a] -> [a] -> Bool

isPermutation [] [] = True 

isPermutation xs xs' | length xs /= length xs' = False
                     | head xs `elem` xs' = isPermutation (tail xs) (delete (head xs) xs')
                

-- Questions
-- Q: Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
-- A: 
