module Exercise3 where
import LTS

-- I first set out to read the paper by Tretmans. But I then opted to skim through it in order to save time.
-- I then got to work on writing the implementation but I just could not figure out how to get the breadth first search working.
-- I simply felt not proficient enough with functional programmning and haskell.
-- Anyway, my plan was to first compose the finite list of traces and afterwards iterate infinitely in breadth over each of the traces to
-- map it to an equivalent trace with delta's inserted in the right spots to produce an infinite list of suspension traces.

findTransitionsByBeforeState :: IOLTS -> State -> [LabeledTransition]
findTransitionsByBeforeState (_, _, _, labeledTransitions, _) state = filter (\(beforeState, _, _) -> beforeState == state) labeledTransitions

transitionIsInput :: LabeledTransition -> [Label] -> Bool
transitionIsInput (_, label, _) = elem label

inputLabelsFromIOLTS :: IOLTS -> [Label]
inputLabelsFromIOLTS (_, inputLabels, _, _, _) = inputLabels

trd :: (a, b, c) -> c
trd (_, _, c) = c

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-------------------------------------------------------------------------

-- Time spent: 360 minutes --
