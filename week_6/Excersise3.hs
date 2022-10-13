module Exercise3 where
import SetOrd
import Data.List

type Rel a = [(a,a)]

setTranspose :: Ord a => Rel a -> Rel a
setTranspose (x : rel) | (fst x == snd x) = setTranspose rel 
                      | otherwise = (snd x, fst x) : setTranspose rel  

setTranspose [] = []

symClos :: Ord a => Rel a -> Rel a
symClos rel = sort(union rel (setTranspose rel)) 

-- Time spent: 60 minutes --
-- TODO: Comments
