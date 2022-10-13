module Exercise4 where
import SetOrd
import Data.List
import Control.Monad
import Test.QuickCheck

type Rel a = [(a,a)]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
 
cartProd :: Arbitrary a => [a] -> [a] -> Gen [(a,a)]
cartProd xs ys = do return [(x,y) | x <- xs, y <- ys]

arbitraryReflexive :: Gen ([Int], Rel Int)
arbitraryReflexive = do
    len <- chooseInt (1, 10)
    d <- vector len >>= \x -> return (nub x) 
    let rel = nub (zip d d)
    return (d, rel)
    
arbitraryDomainRelation :: Gen ([Int], Rel Int)
arbitraryDomainRelation = do
    dLen <- chooseInt (1, 10)
    rLen <- chooseInt (1, 10)
    d <- vector dLen >>= \x -> return (nub x) 
    rel <- cartProd d d >>= (shuffle Control.Monad.>=> (return . take rLen))
    return (d, rel)

checkRelation :: Eq a => a -> [a] -> Rel a -> Bool
checkRelation x d r = any (`elem` r) ([(x, y) | y <- d])

checkAllFirst :: Eq a => [a] -> Rel a -> Bool 
checkAllFirst d (x : rel) | fst x `elem` d = checkAllFirst d rel
                          | otherwise = False

checkAllFirst _ [] = True

isSerial :: Eq a => [a] -> Rel a -> Bool 
isSerial d rel = checkAllFirst d rel && all ((== True) . (\ z -> checkRelation z d rel)) d

propCheckLength :: Eq a => ([a], Rel a) -> Property 
propCheckLength (d, rel) = isSerial d rel ==> length rel >= length d

checkIsSerial :: Eq a => ([a], Rel a) -> Bool
checkIsSerial (d, rel) = isSerial d rel

main = do
    putStrLn "Property 1: isSerial always returns true on reflexive relations."
    quickCheck $ forAll arbitraryReflexive checkIsSerial
    putStrLn "Property 2: length of relation is always greater or equal to the length of domain."
    quickCheck $ forAll arbitraryDomainRelation propCheckLength

-- Time spent: 180 minutes --
-- Test report:
-- Property 1: isSerial always returns true on reflexive relations.
-- +++ OK, passed 100 tests.
-- Property 2: length of relation is always greater or equal to the length of domain.
-- +++ OK, passed 100 tests; 193 discarded.

-- Todo: 4.3 and comments
