module Exercise2 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import System.IO.Unsafe

testAllProperties :: [[Integer] -> Integer -> Bool] -> (Gen [Integer], Integer) -> Gen Bool
testAllProperties (f:xs) a = do 
                                x <- fst a-- Extremely ugly... my Haskell knowledge is not sufficient yet
                                let y = snd a
                                let ok = f x y 

                                if ok 
                                then testAllProperties xs a
                                else return False

testAllProperties [] _ = return True

genLength :: Gen [Bool] -> Gen Int
genLength = fmap (length)

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer 
countSurvivors nMut props f = do
    let nMut' = fromInteger nMut
    let tables = take nMut' [(addElements(f x), x) | x <- [1..]]
    let tested = map (testAllProperties props) tables
    let sequenceTested = sequence tested
    let sequenceFiltered = sequenceTested >>= \x -> return $ filter (== True) x
    let length = genLength sequenceFiltered
    length >>= \x -> return $ toInteger x 
    
main = do
    test <- generate(countSurvivors 4000 multiplicationTableProps multiplicationTable)
    print test