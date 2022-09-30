module Exercise5 where

import Exercise4
import Data.List
import LTS    
import Test.QuickCheck

-- Removes duplicates from a list
-- Retrieved from: https://stackoverflow.com/a/31151043
myNub :: (Eq a) => [a] -> [a]
myNub (x:xs) = x : myNub (filter (/= x) xs)
myNub [] = []

out :: IOLTS -> [State] -> [Label]
out _ [] = []
out lts s | not (null ret) = myNub ret
          | otherwise = ["delta"]
        where
                (states, inLabels, outLabels, transitions, initialState) = lts 
                xs' = filter (\ (x,y,_) -> x `elem` s) transitions
                ret =  filter (`elem` outLabels) [y | (_,y,_) <- xs'] 

t0' = out testModel (after testModel ["coin"]) -- needs to be {!coffee}
t1' = out testModel (after testModel ["but"]) -- needs to be {!tea}
t2' = out testModel (after testModel ["coin", "coffee"]) -- needs to be delta 
t3' = out testModel (after testModel ["coin", "tea"]) -- needs to be [] 

test' = (t0' == ["coffee"]) && (t1' == ["tea"]) && (t2' == ["delta"]) && (t3' == [])

main' = do
        show test'

-------------------------------------------------------------------------
-- We have only implemented the out function
-- to finish the ioco function we can simply define:
-- i ioco s in a function. 

-- Time spent: 300 minutes --

-- test report, running main':
-- "True"