module Exercise5 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Helper function. Assumes that lists are not empty at the start. Assumes lists to be of same size.
isDerangement' :: Eq a => [a] -> [a] -> Bool
isDerangement' (x:xs) (y:ys)
    -- Expect items at current index to not be equal
    | x == y = False
    -- If the tail is empty, then we must have reached the end and the list must be a derangement.
    -- Since we assume that both argument lists are equal size, we don't need to bother with checking both tails. One suffices.
    | null xs = True
    -- So tail was not empty. Then keep iterating.
    | otherwise = isDerangement' xs ys

-- Assumes that empty list is not considered to be a derangement
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys
    -- Check if argument is empty. We don't want the second to be empty either but we don't need to bother with checking that in this
    -- case because the second case handles it.
    | null xs = False
    -- Both lists must have equal size
    | (length xs) /= (length ys) = False
    -- Finally call helper to actually determine if the argument list is a derangement of the other.
    | otherwise = isDerangement' xs ys

-- No need to accept second list because it would need to be empty as well and in that case might as well reuse the first list
propEmptyListIsNotDerangement :: Eq a => [a] -> Property
propEmptyListIsNotDerangement xs = null xs ==> not $ isDerangement xs xs

propUnequalListsAreNotDerangements :: Eq a => [a] -> [a] -> Property
propUnequalListsAreNotDerangements xs ys = (length xs) /= (length ys) ==> not $ isDerangement xs ys

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x ogList) perms
    where ogList = [0..n-1]
          perms = permutations ogList

genEmptyIntList :: Gen [Int]
genEmptyIntList = return []

genIntList :: Gen [Int]
genIntList = arbitrary

-- I had no idea how to further test the function because it already has such little abstraction.
-- Time spent on solution: 1h
-- Time spent on writing tests: 2h
main :: IO()
main = do
    quickCheck $ forAll genEmptyIntList propEmptyListIsNotDerangement;
    quickCheck $ forAll genIntList propUnequalListsAreNotDerangements
