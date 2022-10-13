module Exercise1 where
import SetOrd
import Data.List
import Test.QuickCheck

-- The setUnion function is already given in the setOrd file but we decided to make it ourselves
-- because it was not clear to us if we

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

main :: IO()
main = do
    quickCheck $ forAll setGen propIntersect