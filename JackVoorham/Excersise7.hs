module Excersise7 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

----------------------------
reorderFirstFour :: String -> String
reorderFirstFour (x : y : z : w : str) = str ++ [x] ++ [y] ++ [z] ++ [w] 


iban :: String -> Bool
iban str = mod asInt 97 == 1 where 
    asInt = read $ convertCharsToInteger (reorderFirstFour str)
    
-- Inspired by this logic : https://stackoverflow.com/questions/20659810/haskell-int-to-char
convertCharsToInteger :: String -> String 
convertCharsToInteger [] = []
convertCharsToInteger (x : str) | ord x >= 65 && ord x <= 95 = show (ord x - 55) ++ convertCharsToInteger str
                                | otherwise = x : convertCharsToInteger str

-----------------------------