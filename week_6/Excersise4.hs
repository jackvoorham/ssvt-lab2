module Excersise4 where
import SetOrd

type Rel a = [(a,a)]

-- A relation R is serial on a domain A if for any x ∈ A there is an y ∈ A such that xRy. Suppose relations are represented as lists of pairs:

checkRelation :: Eq a => a -> [a] -> Rel a -> Bool
checkRelation x d r = any (`elem` r) ([(x, y) | y <- d])

isSerial :: Eq a => [a] -> Rel a -> Bool 
isSerial x y = all ((== True) . (\ z -> checkRelation z x y)) x

-- Should be false
checkFalse = isSerial [1,2,3] [(1,3), (1,2), (2,3)]

-- Should be true
checkTrue = isSerial [1,2,3] [(1,3), (1,2), (2,3), (3,1)]
 