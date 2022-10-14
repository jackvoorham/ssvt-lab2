module Exercise4 where
import SetOrd
import Data.List
import Control.Monad
import Test.QuickCheck

type Rel a = [(a,a)]

-- Returns the cartesian product of two lists
cartProd :: Arbitrary a => [a] -> [a] -> Gen [(a,a)]
cartProd xs ys = do return [(x,y) | x <- xs, y <- ys]

-- Generators for arbritrary reflexive relations
arbitraryReflexive :: Gen ([Int], Rel Int)
arbitraryReflexive = do
    len <- chooseInt (1, 10)
    d <- vector len >>= \x -> return (nub x) 
    let rel = nub (zip d d)
    return (d, rel)

-- Generator for an arbritary donain and arbritrary entries xRy
arbitraryDomainRelation :: Gen ([Int], Rel Int)
arbitraryDomainRelation = do
    dLen <- chooseInt (1, 10)
    rLen <- chooseInt (1, 10)
    d <- vector dLen >>= \x -> return (nub x) 
    rel <- cartProd d d >>= (shuffle Control.Monad.>=> (return . take rLen))
    return (d, rel)

-- isSerial: checks if a relation is serial
-- we first check all of the entries in the domain are a x in the relations xRy.
-- then we check whether it indeed holds that for each x there is a xRy with y in
-- the domain
isSerial :: Eq a => [a] -> Rel a -> Bool 
isSerial d rel = checkAllFirst d rel && all ((== True) . (\ z -> checkRelation z d rel)) d

checkAllFirst :: Eq a => [a] -> Rel a -> Bool 
checkAllFirst d (x : rel) | fst x `elem` d = checkAllFirst d rel
                          | otherwise = False

checkAllFirst _ [] = True

checkRelation :: Eq a => a -> [a] -> Rel a -> Bool
checkRelation x d r = any (`elem` r) ([(x, y) | y <- d])

-- Property of seriality: if serial relation, the length of R must be >= then the 
-- length of the domain
propCheckLength :: Eq a => ([a], Rel a) -> Property 
propCheckLength (d, rel) = isSerial d rel ==> length rel >= length d

-- Helper to check if serial, used for testing reflexive relations
checkIsSerial :: Eq a => ([a], Rel a) -> Bool
checkIsSerial (d, rel) = isSerial d rel

main = do
    putStrLn "Property 1: isSerial always returns true on reflexive relations."
    quickCheck $ forAll arbitraryReflexive checkIsSerial
    putStrLn "Property 2: length of relation is always greater or equal to the length of domain."
    quickCheck $ forAll arbitraryDomainRelation propCheckLength

-- Time spent: 240 minutes --

-- 4.b: 
-- We have tested two properties of isSerial, the first one is that relfexive relations should always be
-- serial. This is the case because if x is in the domain, then (x,x) is in the relation, and because
-- for seriality we need to have relations (x,y) where x is in the domain and y is in the domain, in the case
-- reflexive relations we can substitute y for x. And as we already shown that x is in the domain we can
-- conslude that it is serial.

-- The second property we have tested is that the length of a relation must always be greater or equal to 
-- the length of the domain. This is the case because each element in the domain should map to at least one output, thus there should
-- be at least one xRy in R. Thus, R must be at least as large as the domain. 

-- Test report:
-- Property 1: isSerial always returns true on reflexive relations.
-- +++ OK, passed 100 tests.
-- Property 2: length of relation is always greater or equal to the length of domain.
-- +++ OK, passed 100 tests; 193 discarded.

-- 4.c
-- R = {(x, y) | x = y(mod n)} 
-- We know from the property described in 4.b that reflexive relations are always serial
-- We also know that a property of the modulo operator is that when x = (y mod n) and n 
-- is a number greater then y, then x = y. Thus we can conclude that R is indeed serial
-- when the relation only contains entry where (x,y) and x = y, and n > x (and thus also > y).
