module Exercise4 where

import Data.List
import LTS    
import Test.QuickCheck
import Debug.Trace

-- Implementation of after
-- Parameters: IOLTS and label
-- We must follow the label from the initial state
-- then when we have traversed the path, we can append q' 
-- of the last transition (q -> q') we found 
                
getQ' :: [LabeledTransition] -> Label -> State -> [State]
getQ' xs l initialState = [i | (_,_,i) <- xs']
        where
                xs' = filter (\ (x, y, _) -> (y == l) && (x == initialState)) xs

after' :: [Label] -> LTS -> [[State]]
after' [] r = []
after' l r = qPrimes : concatMap (after' tailed) ([(states, labels, transitions, i) | i <- qPrimes])
        where 
                (states, labels, transitions, initialState) = r
                label = head l
                tailed = tail l
                qPrimes = getQ' transitions label initialState -- one

after :: LTS -> [Label] -> [State]
after r l = last (after' l r)

tt = after tretmanR ["but", "but", "choc"]
-- ([0,1,2,3,4,5],["but","choc","liq"],[(0,"but",1),(0,"but",2),(1,"liq",3),(2,"but",4),(4,"choc",5)],0)