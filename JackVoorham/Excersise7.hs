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

test = all iban correctIBANS
-----------------------------
-- Time spent: 2 hours