module Exercise6 where
import Test.QuickCheck
import Exercise3

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

propTrClos' :: Ord a => Rel a -> Rel a -> Bool
propTrClos' (entry:rest) trClosOfRel =

main :: IO()
main = do
  quickCheck $ forAll (arbitrary :: Gen (Rel Int)) propSymClos
  quickCheck $ forAll (arbitrary :: Gen (Rel Char)) propSymClos
