module Exercise3 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Functor
import Debug.Trace
import Control.Monad

type Input = Integer
type OutputItem = Integer
type Output = [OutputItem]
type Fut = Input -> Output
type Prop = Output -> Input -> Bool
type Mutator = (Output -> Gen Output)

-- I started by implementing a small function which parses the result from mutate' of Gen [Bool] into a more simple Gen Bool.
-- I then expanded this towards the assignment requirement of the function.
-- I had issues with combining multiple sets of props with multiple mutators. Things like 'find'
-- do not work on monads so I had to find or even write up my own work-arounds.
-- I'm apparently also really bad at haskell because I would get compiler errors every 2 words. This made the
-- time spent blow up.
-- Results on multiplication table props: an unspecified 2 of the 5 props are sufficient. In order to make the props
-- identifiable, I would perhaps need to process props as 2-tuples in order to store an id next to the function.
-- Time spent: 12h
minPropSubsets :: [Prop] -> Fut -> Gen [Prop]
minPropSubsets props = minPropSubsets' propSubsets
  -- sort in order to have subsets ordered from small to large and thus find the minimum propset quicker
  -- tail in order to discard empty set
  where propSubsets = quicksort $ tail $ subsets props

-- Variant which accepts a set of prop sets
minPropSubsets' :: [[Prop]] -> Fut -> Gen [Prop]
minPropSubsets' [] fut = return []
minPropSubsets' (props:propss) fut = do
  killedMutants <- propsKillMutants mutators props fut
  if killedMutants then return props else minPropSubsets' propss fut

-- Helper function which determines if a set of props kills all mutants from a set of mutators
propsKillMutants :: [Mutator] -> [Prop] -> Fut -> Gen Bool
propsKillMutants mutators props fut = do
  input <- arbitrary :: Gen Integer
  mutationResult <- mapM (\mutator -> mutate' mutator props fut input) mutators
  let flattenedMutationResult = do join mutationResult
  return $ (not . and) flattenedMutationResult

-- https://stackoverflow.com/questions/52674974/haskell-generating-subsets-of-length-k
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = [zs | ys <- subsets xs, zs <- [ys, x:ys]]

-- https://stackoverflow.com/a/19083491
-- Works on list of lists. Orders by list length.
quicksort :: [[a]] -> [[a]]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (\x -> (length x) < (length p) ) xs
        greater = filter (\x -> (length x) >= (length p) ) xs
