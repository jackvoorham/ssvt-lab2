module Exercise1 where
import Data.List
import LTS    
import Test.QuickCheck
import Debug.Trace

-- Factors that would make a IOLTS not valid
-- 1. If >= 1 of the of the input actions is not enabled in >= 1 states

-- For a valid IOLTS/LTS, we have:
-- Q is a countable, NON-EMPTY set of states;
-- L is a countable set of labels
---- Can be devided in stimulus and response with ? and ! respectively
-- T, the transition relation, contains elements of Q and labels of L
-- q_0, the initial state, a element of Q


validateTransitions :: [State] -> [Label] -> [Label] -> [LabeledTransition] -> Bool

validateTransitions states inLabels outLabels [] = True

validateTransitions states inLabels outLabels transitions | correctLabels && correctStates = validateTransitions states inLabels outLabels (tail transitions)
                                                          | otherwise = False
    where 
    (inState, label, outState) = head transitions
    correctStates = (inState `elem` states) && (outState `elem` states)
    correctLabels = (label `elem` inLabels) || (label `elem` outLabels) 

validateLTS :: IOLTS -> Bool 
validateLTS (states, inLabels, outLabels, transitions, initialState) | null states = False
                                                                     | ((initialState `elem` states) && (validateTransitions states inLabels outLabels transitions)) = True 
                                                                     | otherwise = False

test = all validateLTS [coffeeImpl1, coffeeImpl2, coffeeImpl3, coffeeImpl4, coffeeImpl5, coffeeImpl6]


