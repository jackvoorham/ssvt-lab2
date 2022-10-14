module Exercise6 where
import Test.QuickCheck
import Exercise3
import Exercise5 (trClos)
import SetOrd
import Data.List

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- We noticed first that the Rel type has a structure that is suitable for use with haskell function 'lookup'.
-- However the 'lookup' function is not exactly suitable here because it returns only the first match. So
-- We wrote lookupAll which returns all matches. This allows propSymClos to check if, from a given pair (k,v)
-- from a relationship,
-- pair (v,k) also exists, in which the symmetric property for that pair would hold.
-- To test the trClos function, we check if the output contains all the transitive states from {output @@ input} plus the input.
-- (This is because the output should contain the input plus all transitive relations).
-- We also test the property that the output should be the same length as the input plus {output @@ input}.
-- One thing we needed to pay attention to is the fact that quickcheck with the Gen Rel Int can also generate lists
-- that containt duplicate elements so we filter those out.
-- Time spent: 110m
-- Test report:
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll key [] = []
lookupAll key ((k, v):rest) = iterationResult ++ (lookupAll key rest)
  where iterationResult = if key == k then [v] else []

propSymClos :: Ord a => Rel a -> Bool
propSymClos rel = propSymClos' rel symClosOfRel
  where symClosOfRel = symClos rel

propSymClos' :: Ord a => Rel a -> Rel a -> Bool
propSymClos' [] symClosOfRel = True
propSymClos' ((k,v):rest) symClosOfRel = if k `elem` (lookupAll v symClosOfRel) then propSymClos' rest symClosOfRel else False

propTrClos :: Ord a => Rel a -> Bool
propTrClos rel = contains (trClos rel) (nub (((trClos rel) @@ rel) ++ rel))

propTrClos' :: Ord a => Rel a -> Bool
propTrClos' rel = length (trClos rel) == length (nub (((trClos rel) @@ rel) ++ rel))

contains :: Ord a => Rel a -> Rel a -> Bool
contains rel [] = True
contains rel (x:xs)
  | elem x rel = contains rel xs
  | otherwise = False

main :: IO()
main = do
  quickCheck $ forAll (arbitrary :: Gen (Rel Int)) propSymClos
  quickCheck $ forAll (arbitrary :: Gen (Rel Char)) propSymClos
  quickCheck $ forAll (arbitrary :: Gen (Rel Int)) propTrClos
  quickCheck $ forAll (arbitrary :: Gen (Rel Int)) propTrClos'