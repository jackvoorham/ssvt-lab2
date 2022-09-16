module Excersise1 where
import Data.List
import Data.Char
import System.Random
import System.IO.Unsafe
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

-- Check quartile ranges of lists of size 10000. Filters the list in subarrays of quartiles and checks
-- whether the length is in specificied range If true we can say the lists's quartiles are evently 
-- distributed w.r.t to the specified range
checkQuartileRanges :: [Float] -> Property
checkQuartileRanges xs = length xs == 10000 ==> check where
    range = [2300..2700] -- The accepted range
    i = length $ filter (\x -> x >= 0 && x <= 0.25) xs
    j = length $ filter (\x -> x >= 0.25 && x <= 0.5) xs
    k = length $ filter (\x -> x >= 0.5 && x <= 0.75) xs   
    l = length $ filter (\x -> x >= 0.75 && x <= 1) xs
    check = i `elem` range && j `elem` range && k `elem` range && l `elem` range
    
-- Generates only positive integers, but modified to make it generate only 10000
-- From: https://stackoverflow.com/a/39292322
genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (== 10000)

-- Tests the probs function distribution.  Parameter x is the length we want to test, for us this is 10000
probsTest x = check
    where     
        xs = unsafePerformIO(probs x)
        check = checkQuartileRanges xs

main = do 
    quickCheck $ forAll genPos probsTest

-----------------------------

-- Test rapport (Checked with [2300-2700] as the accepted range): 
-- +++ OK, passed 100 tests.