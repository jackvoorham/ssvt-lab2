import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace

testAllProperties :: [[Integer] -> Integer -> Bool] -> ([Integer], Integer) -> Bool
testAllProperties (f:xs) a | ok = testAllProperties xs a 
                           | otherwise = False
                           where 
                               x = fst a
                               y = snd a
                               ok = f x y 
                               
testAllProperties [] _ = True

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Bool 
countSurvivors x y f = ok where
    x' = fromInteger x 
    xs = take x' [(f x, x) | x <- [1..]]
    xs' = map (testAllProperties y) xs
    ok = all (==True) xs'
    
test = countSurvivors 4000 multiplicationTableProps multiplicationTable  
