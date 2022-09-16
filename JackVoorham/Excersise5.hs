module Excersise5 where
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

isPermutation xs xs' | head xs `elem` xs' = isPermutation (tail xs) (delete (head xs) xs')
                     | null xs = True
                     | otherwise = False
                   
test = isPermutation [1,2,3] [3,2,1]