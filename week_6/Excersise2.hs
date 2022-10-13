module Exercise1 where
import SetOrd
import Data.List
import Test.QuickCheck

-- The union function is already given in the setOrd file. Because it is not clear whether we should
-- implement all the functions ourselves, we decided to do both.
-- The normal function is our own implementation and the setUnion' function uses the given union function.

setIntersection ::   Ord a => Set a -> Set a -> Set a
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

setGen :: Gen (Set Integer)
setGen = do
    range <- choose(1, 10) :: Gen Integer
    let lstSet = list2set [range..(range+3)]
    return lstSet

propIntersect :: Ord a => Set a -> Set a -> Bool
propIntersect  set1 set2 = subSet (setIntersection (set1) (set2)) set1 && subSet (setIntersection (set1) (set2)) set2

-- main :: IO()
-- main = do
--     quickCheck $ forAll setGen propIntersect