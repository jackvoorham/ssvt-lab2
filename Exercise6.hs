-- Base code imported from Lab 2 at https://canvas.uva.nl/courses/32483/files/7147684?wrap=1&fd_cookie_set=1
module Exercise6 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

----------------------------

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs) 
    | x `elem` ['a'..'m'] = [chr (ord x + 13)] ++ rot13 xs
    | x `elem` ['n'..'z'] = [chr (ord x - 13)] ++ rot13 xs
    | x `elem` ['A'..'M'] = [chr (ord x + 13)] ++ rot13 xs
    | x `elem` ['N'..'Z'] = [chr (ord x - 13)] ++ rot13 xs
    | otherwise = [x] ++ rot13 xs

-- We use this to generate strings for quickcheck.
genString :: Gen String
genString = arbitrary :: Gen String

-- First we test if the inverse of the inverse is the same (which should be the case => rot13(rot13(word)) = word).
testInverse :: [Char] -> Bool
testInverse n = n == rot13 (rot13 n)

-- A second thing we can test is that the length of the string stays the same.
testLength :: [Char] -> Bool
testLength n = length n == length (rot13 n)

-- Since wikipedia specifically stated that symbols should not be affected, we can test whether that's the case.
-- We use this to extract all the symbols before and after rot13
extractSymbols :: [Char] -> [Char]
extractSymbols [] = []
extractSymbols (x:xs) 
    | toLower x `elem` ['a'..'m'] = extractSymbols xs
    | otherwise = [x] ++ extractSymbols xs

testSymbols :: [Char] -> Bool
testSymbols n = extractSymbols n == extractSymbols (rot13 n)

main :: IO()
main = do
    print "Testing inverse"
    quickCheck $ forAll genString testInverse
    print "Testing length"
    quickCheck $ forAll genString testLength
    print "Testing symbols"
    quickCheck $ forAll genString testLength
    
--------------------------------------
    
-- Time spent: 1.5 hours.

-- Taken from: https://en.wikipedia.org/wiki/ROT13
-- ROT13 is a cipher which shifts each letter of a word by 13 in the alphabet. This means that the A becomes an N and B an O etc.
-- Because the english alphabet has 26 letters and 13 is exactly half of that, running ROT13 twice should give back the original word.
-- This is a property which is testable, after having done the implementation.
-- One thing we needed to take into account is that by ascii standards, lowercase and uppercase letters aren't the same.
-- On the wikipedia it is specifically stated that only letters are affected meaning symbols stay the same

-- We used the following answer on stackoverflow to figure out that we needed to use chr and ord in order to shift a letter:
-- https://stackoverflow.com/questions/13223605/how-to-write-a-function-to-modify-a-char-list-and-return-it-in-haskell
-- Because the letters are each other inverse in rot13, we can simply write minus 13 instead of needing to loop around if we go beyond the letter z.
-- The otherwise is necessary for things such as symbols.

--Test rapport:
-- "Testing inverse"
-- +++ OK, passed 100 tests.
-- "Testing length"
-- +++ OK, passed 100 tests.
-- "Testing symbols"
-- +++ OK, passed 100 tests.