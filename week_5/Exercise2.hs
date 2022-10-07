module Exercise2 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import System.IO.Unsafe

testAllProperties :: [[Integer] -> Integer -> Bool] -> (Gen [Integer], Integer) -> Bool
testAllProperties (f:xs) a = ok && testAllProperties xs a
                           where 
                               x = unsafePerformIO (generate (fst a)) -- Extremely ugly... my Haskell knowledge is not sufficient yet
                               y = snd a
                               ok = f x y 

testAllProperties [] _ = True

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer 
countSurvivors x y f = do
    let x' = fromInteger x 
    let tables = take x' [(addElements(f x), x) | x <- [1..]]
    let tested = map (testAllProperties y) tables
    let survivors = length(filter (==True) tested)
    return (toInteger survivors)
    
test = countSurvivors 4000 multiplicationTableProps multiplicationTable  