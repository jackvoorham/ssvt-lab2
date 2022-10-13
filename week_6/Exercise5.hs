module Exercise5 where
import SetOrd
import Data.List

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- We first noticed that the new @@ infix can be combined with the original list by
-- doing @@ on itself (so original rel @@ original rel) which gives the first paths.
-- This process can then be repeated with the new set of original rel + new paths.
-- This has to be done untill no further transitive relations can be found.
-- For this we created the insertRel function which inserts a single Rel into a Rel.
-- We also created the insertRelList which takes multiple Rels and inserts them into another Rel.
-- Time spend: 80 minutes --

insertRel :: Ord a => Rel a -> Rel a -> Rel a
insertRel rel1 [] = rel1
insertRel rel1 (x:xs)
    | (fst $ head rel1) < fst x = nub (rel1 ++ (x:xs))
    | (fst $ head rel1) == fst x && (snd $ head rel1) < snd x = nub (rel1 ++ (x:xs))
    | otherwise = nub ([x] ++ insertRel rel1 xs)

insertRelList :: Ord a => Rel a -> Rel a -> Rel a
insertRelList [] relList = relList
insertRelList (x:xs) relList = insertRelList (xs) (insertRel [x] relList)

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos rel1
    | rel1 == (insertRelList (rel1 @@ rel1) rel1) = rel1
    | otherwise = trClos (insertRelList (rel1 @@ rel1) rel1)
