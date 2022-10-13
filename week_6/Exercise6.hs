module Exercise6 where
import Test.QuickCheck
import Exercise3

-- I noticed first that the Rel type has a structure that is suitable for use with haskell function 'lookup'.
-- However the 'lookup' function is not exactly suitable here because it returns only the first match. So
-- I wrote lookupAll which returns all matches. This allows propSymClos to check if, from a given pair (k,v)
-- from a relationship,
-- pair (v,k) also exists, in which the symmetric property for that pair would hold.
-- Time spent: 90m
-- Test report:
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

main :: IO()
main = do
  quickCheck $ forAll (arbitrary :: Gen (Rel Int)) propSymClos
  quickCheck $ forAll (arbitrary :: Gen (Rel Char)) propSymClos
