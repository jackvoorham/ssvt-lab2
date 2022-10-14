module Exercise3 where
import SetOrd
import Data.List

type Rel a = [(a,a)]

-- Tranposess a list of relations
setTranspose :: Ord a => Rel a -> Rel a
setTranspose (x : rel) | uncurry (==) x = setTranspose rel 
                      | otherwise = (snd x, fst x) : setTranspose rel  

-- Base case for setTranspose
setTranspose [] = []

-- Returns the symmetric relation of a relation
symClos :: Ord a => Rel a -> Rel a
symClos rel = sort(rel `union` setTranspose rel) 

-- Time spent: 60 minutes --
