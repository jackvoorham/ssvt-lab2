module Exercise2 where
    
import Exercise1
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import System.IO.Unsafe

genLength :: Gen [Bool] -> Gen Int
genLength = fmap (length)

testAllProperties :: [[Integer] -> Integer -> Bool] -> (Gen [Integer], Integer) -> Gen Bool
testAllProperties (f:xs) a = do 
                                x <- fst a
                                let y = snd a
                                let ok = f x y 

                                if ok 
                                then testAllProperties xs a
                                else return False

testAllProperties [] _ = return True

countSurvivors :: Integer -> ([Integer] -> Gen [Integer]) -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer 
countSurvivors nMut mutF props f = do
    let nMut' = fromInteger nMut
    let tables = take nMut' [(mutF(f x), x) | x <- [1..]]
    let tested = map (testAllProperties props) tables
    let sequenceTested = sequence tested
    let sequenceFiltered = sequenceTested >>= \x -> return $ filter (== True) x
    let length = genLength sequenceFiltered
    length >>= \x -> return $ toInteger x 
    
main = do
    testAdd <- generate(countSurvivors 4000 addElements multiplicationTableProps multiplicationTable)
    testRemove <- generate(countSurvivors 4000 removeElements multiplicationTableProps multiplicationTable)
    testScramble <- generate(countSurvivors 4000 scrambleElements multiplicationTableProps multiplicationTable)
    testReverse <- generate(countSurvivors 4000 reverseElements multiplicationTableProps multiplicationTable)

    putStrLn "Testing countSurvivors with addElements mutator... Number of survivors is:"
    print testAdd
    putStrLn "Testing countSurvivors with removeElements mutator... Number of survivors is:"
    print testRemove
    putStrLn "Testing countSurvivors with scrambleElements (from ex. 1) mutator... Number of survivors is:"
    print testScramble
    putStrLn "Testing countSurvivors with reverseElements (from ex. 1) mutator... Number of survivors is:"
    print testReverse

-- Time spent: 360 minutes --

-- Output of main:

{--
Testing countSurvivors with addElements mutator... Number of survivors is:
0
Testing countSurvivors with removeElements mutator... Number of survivors is:
41
Testing countSurvivors with scrambleElements (from ex. 1) mutator... Number of survivors is:
0
Testing countSurvivors with reverseElements (from ex. 1) mutator... Number of survivors is:
0
--}

-- From this we see that the number of survivors is the highest when we use the removeElements mutator

-- Documentation of approach:
-- For the implementation i have just implemented the countSurvivors function with the specified type signature in the assignment
-- We first start with create the mutated tables on line 29, we simply take the number of muttions we want from the list of mutations of multiplication
-- tables, then we pass each of the mutated multiplication tables to the testAllProperties function which recursively applies all the properties, and 
-- results in True if all properties were checked succesfully. The results of applying the testAllProperties function on all of the created mutated
-- multiplication tables gets put into a list of booleans with map. 
-- We then finally filter this list so that it only contains True, that is we passed all the tests, and we return the length of this list. The length of 
-- the truth list is ofcourse the number of survivors, because this ultimately is the number of mutations that passed all the properties. 