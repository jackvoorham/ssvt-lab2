-- Github: https://github.com/jackvoorham/ssvt-lab2

module Exercise4 where
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

-- The first property; When provided with the same list we must have a permutation by defintion
-- because duplicating the list does not change the length and elements included in the list
propertySameList :: Eq a => [a] -> Bool
propertySameList xs = isPermutation xs xs

-- The first property; When provided with list and the reversed list we must have a permutation by defintion
-- because reversing the list does not change the length and elements included in the list
propertyReversedList :: Eq a => [a] -> Bool
propertyReversedList xs = isPermutation xs (reverse xs)


-- The first property; When provided with the list and the tailed list we must not have a permutation by defintion
-- because tailing the list changes the length to n-1 and removes the first element and thus does not have the same elements
propertyTailedList :: Eq a => [a] -> Bool
propertyTailedList xs = not (isPermutation xs (tail xs))

-- Arbitrary list generator for non-empty lists
-- From: https://www.stackbuilders.com/blog/a-quickcheck-tutorial-generators/
-- Modified to make it generate only non-empty lists
arbitraryList :: Arbitrary a => Gen [a]
arbitraryList =
  sized $
    \n -> do
      k <- choose (1, n)
      sequence [ arbitrary | _ <- [0..k] ]

-- We test the property's on integer list, if we test on it it should work with any type because of parametricity
-- Src: https://stackoverflow.com/a/7149859
main = do
    putStrLn "Testing propertySameList"
    quickCheck (propertySameList :: [Int] -> Bool)
    putStrLn "Testing propertyReversedList"
    quickCheck (propertyReversedList :: [Int] -> Bool)
    putStrLn "Testing propertyTailedList"
    quickCheck $ forAll arbitraryList (propertyTailedList :: [Int] -> Bool)

----------------------------

-- Time spent: 3 hours

-- Questions
-- Q: Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
-- A: It does not impact my testing procedure, as the non-duplicate lists are a subset of all lists, so it must also hold that they work if we have tests that work when generating all lists

-- Q: Provide an ordered list of properties by strength using the weaker and stronger definitions.
-- A: For my properties the list would be as follows, from weakest to strongest -> [propertySameList, propertyReversedList, PropertyTailedList] as the PropertyTailedList is more restrictive as it does not accept the empty list

-- Test rapport:
-- Testing propertySameList
-- +++ OK, passed 100 tests.
-- Testing propertyReversedList
-- +++ OK, passed 100 tests.
-- Testing propertyTailedList
-- +++ OK, passed 100 tests.
