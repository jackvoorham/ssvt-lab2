module Exercise1 where
import SetOrd
import Data.List
import Test.QuickCheck

-- The union function is already given in the setOrd file. Because it is not clear whether we should
-- implement all the functions ourselves, we decided to do both.
-- The normal function is our own implementation and the setUnion' function uses the given union function.
-- In order to test we had to write our own generator to use with quickcheck which took a considerable amount of time.
-- To test intersect we test if the intersect is a subset of both sets.
-- To test union we test is both sets are subsets of the union.
-- To test the difference we need to first test the edge cases:
-- If the first set is empty or if the first set is a subset of the second set,
-- the difference will be the empty set which will always be a subset of the second set.
-- In that case we can simply test if the difference is the empty set.
-- In other cases we test if the difference is a subset of set 1 but not of set 2.
-- Time spend: 250 minutes --

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = Set []
setIntersection (Set (x:xs)) set2
    | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
    | otherwise = setIntersection (Set xs) set2

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []) set2 = set2
setUnion (Set (x:xs)) set2
    | inSet x set2 = setUnion (Set xs) set2
    | otherwise = insertSet x (setUnion (Set xs) set2)

setUnion' :: Ord a => Set a -> Set a -> Set a
setUnion' set1 set2 = unionSet set1 set2

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = Set []
setDifference (Set (x:xs)) set2
    | inSet x set2 && inSet x (setIntersection (Set(x:xs)) set2) = setDifference (Set xs) set2
    | otherwise = insertSet x (setDifference (Set xs) set2)

setGen :: (Arbitrary a, Ord a) => Gen (Set a)
setGen = do
    list <- arbitrary
    return $ list2set list

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = setGen

propIntersect :: Set Int -> Set Int -> Bool
propIntersect set1 set2 = and [subSet intersect set1, subSet intersect set2]
  where intersect = setIntersection set1 set2

propUnion :: Set Int -> Set Int -> Bool
propUnion set1 set2 = and [subSet set1 union, subSet set2 union]
  where union = setUnion set1 set2

propUnion' :: Set Int -> Set Int -> Bool
propUnion' set1 set2 = and [subSet set1 union, subSet set2 union]
  where union = setUnion' set1 set2

propDifference :: Set Int -> Set Int -> Bool
propDifference set1 set2 
  | isEmpty set1 = isEmpty (setDifference set1 set2)
  | subSet set1 set2 = isEmpty (setDifference set1 set2)
  | otherwise = and [subSet difference set1, not (subSet difference set2)]
      where difference = setDifference set1 set2

main :: IO()
main = do 
  quickCheck propIntersect
  quickCheck propUnion
  quickCheck propUnion'
  quickCheck propDifference
