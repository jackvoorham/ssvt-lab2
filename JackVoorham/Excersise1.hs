module Excersise1 where
import Data.List
import Data.Char
import System.Random

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

----------------------------

checkQuartileTuple :: [Float] -> Bool
checkQuartileTuple xs = check where
    range = [2400..2600] -- The accepted range
    i = length $ filter (\x -> x >= 0 && x <= 0.25) xs
    j = length $ filter (\x -> x >= 0.25 && x <= 0.5) xs
    k = length $ filter (\x -> x >= 0.5 && x <= 0.75) xs   
    l = length $ filter (\x -> x >= 0.75 && x <= 1) xs
    check = i `elem` range && j `elem` range && k `elem` range && l `elem` range


main :: IO()
main = do 
    xs <- probs 10000
    let result = checkQuartileTuple xs
    print(result)
