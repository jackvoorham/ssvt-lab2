module Excersise7 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

----------------------------
correctIBANS = ["AL35202111090000000001234567", "AD1400080001001234567890", "AT483200000012345864", "AZ77VTBA00000000001234567890", "BH02CITI00001077181611",
                "BY86AKBB10100000002966000000", "BE71096123456769", "BA393385804800211234", "BR1500000000000010932840814P2", "BG18RZBB91550123456789", "BI43220001131012345678912345",
                "CR23015108410026012345", "HR1723600001101234565", "CY21002001950000357001234567", "CZ5508000000001234567899", "DK9520000123456789", "DO22ACAU00000000000123456789",
                "EG800002000156789012345180002", "SV43ACAT00000000000000123123", "EE471000001020145685", "FO9264600123456789", "FI1410093000123458", "FR7630006000011234567890189"]

-- Function reorders the first 4 characters to be reordered to the end of the passed string
reorderFirstFour :: String -> String
reorderFirstFour (x : y : z : w : str) = str ++ [x] ++ [y] ++ [z] ++ [w] 

-- Checks if passed string is a correct IBAN
iban :: String -> Bool
iban str = mod asInt 97 == 1 where 
    asInt = read $ convertCharsToInteger (reorderFirstFour str)
    
-- Inspired by this logic : https://stackoverflow.com/questions/20659810/haskell-int-to-char
-- We check if the character in the string is between "A" and "Z" if so we transform it to 
-- the needed integer by subtracting 55 from the converted character to a integer, which puts 
-- us in the correct range of numbers we need to map to, that is the range 10 to 35. 
-- We then recursively add the converted character to the string we are building, if it is not 
-- in the range from "A" to "Z" we simply append it unchanged
convertCharsToInteger :: String -> String 
convertCharsToInteger [] = []
convertCharsToInteger (x : str) | ord x >= 65 && ord x <= 95 = show (ord x - 55) ++ convertCharsToInteger str
                                | otherwise = x : convertCharsToInteger str

main = do
    all iban correctIBANS
-----------------------------
-- Time spent: 2 hours

-- To automate the testing process one should write a generator that creates correct IBAN numbers, to create incorrect ones one can mutate correct IBAN's slightly
-- One can base such generator on the ISO's of the IBAN specification