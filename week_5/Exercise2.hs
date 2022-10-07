import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace

testAllProperties :: [[Integer] -> Integer -> Bool] -> ([Integer], Integer) -> Bool
testAllProperties (f:xs) a = if ok
                             then testAllProperties xs a 
                             else False
                           where 
                               x = fst a
                               y = snd a
                               ok = f x y 

testAllProperties [] _ = True

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Bool 
countSurvivors x y f = do
    let x' = fromInteger x 
    let xs = take x' [(f x, x) | x <- [1..]]
    let xs' = map (testAllProperties y) xs
    let ok = all (==True) xs' 
    return ok
    
test = countSurvivors 4000 multiplicationTableProps multiplicationTable  