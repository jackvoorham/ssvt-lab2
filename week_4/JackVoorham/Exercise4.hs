module Exercise4 where

import Data.List
import LTS    
import Test.QuickCheck

-- Gets q', that is the states we can go to in the current state and taking the label
getQ' :: [LabeledTransition] -> Label -> State -> [State]
getQ' xs l initialState = [i | (_,_,i) <- xs']
        where
                xs' = filter (\ (x, y, _) -> (y == l) && (x == initialState)) xs

-- Removes the delta's from a list, as they are indefferent
-- to the outcome of the path traversal
removeDelta :: [Label] -> [Label]
removeDelta l = [x | x <- l, x /= "delta"]

-- Recurses through all labels and then returns when we
-- reach an empty list of labels, that is, we have traversed all labels
after' [] r = []
after' l r = qPrimes : concatMap (after' tailed) ([(states, inLabels, outLabels, transitions, i) | i <- qPrimes])
        where 
                (states, inLabels, outLabels, transitions, initialState) = r
                label = head l
                tailed = tail l
                qPrimes = getQ' transitions label initialState -- Get the qPrimes in the current state and with the current label

-- Take head of the after' return, that is, the last set of q' we found
after :: IOLTS -> [Label] -> [State]
after r l = last (after' l' r)
        where 
                l' = removeDelta l -- Remove the deltas as they do not matter for following the trace

        
testModel = createIOLTS [(1, "?but", 2), (1, "?coin", 3), (2, "!tea", 4), (3, "!coffee", 5), (3, "!coffee", 6)]

t0 = after testModel ["coin"] -- needs to be [3]
t1 = after testModel ["but"] -- needs to be [2]
t2 = after testModel ["coin", "coffee"] -- needs to be [5,6] 
t3 = after testModel ["coin", "tea"] -- needs to be [] 

test = (t0 == [3]) && (t1 == [2]) && (t2 == [5,6]) && (t3 == [])

main = do
        show test

-------------------------------------------------------------------------
-- Time spent: 300 minutes --

-- test report, running main:
-- "True"