module Exercise7 where
import Test.QuickCheck
import Exercise3 (symClos)
import Exercise4 (Rel)
import Exercise5 (trClos)

-- We knew that we did not have to deliver code for this exercise but it made it so easy
-- to test the theory so we wrote the code anyway.
-- Results:
-- *** Failed! Falsified (after 4 tests):
-- [(0,3)]
-- This makes sense. symClos $ trClos [(0,3)] results in [(0,3),(3,0)], while trClos $ symClos results in
-- [(0,0),(0,3),(3,0),(3,3)] because after the symClos, both 0 and 3 will have an indirect path to themselves.
-- Time spent: 20m

propClosureFormulasAreEq :: Ord a => Rel a -> Bool
propClosureFormulasAreEq rel = fa == fb
  where fa = symClos $ trClos rel
        fb = trClos $ symClos rel

main :: IO()
main = quickCheck $ forAll (arbitrary :: Gen (Rel Int)) propClosureFormulasAreEq
