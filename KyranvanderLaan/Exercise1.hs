-- Base code imported from Lab 2 at https://canvas.uva.nl/courses/32483/files/7147684?wrap=1&fd_cookie_set=1

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

-- https://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
fourTupleMap :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
fourTupleMap f (a1,a2,a3,a4) = (f a1, f a2, f a3, f a4)

testProbs :: [Float] -> (Float, Float, Float, Float) -> (Float, Float, Float, Float) 
testProbs [] (a,b,c,d) = fourTupleMap (/ (a+b+c+d)) (a,b,c,d)
testProbs (x:xs) (a, b, c, d) = if x < 0.25
                                    then testProbs (xs) (a+1,b,c,d)
                                    else if x < 0.5
                                        then testProbs (xs) (a,b+1,c,d)
                                        else if x < 0.75
                                            then testProbs (xs) (a,b,c+1,d)
                                            else testProbs (xs) (a,b,c,d+1)

main :: IO (Float, Float, Float, Float)
main = do
    temp <- probs 10000
    return (testProbs temp (0,0,0,0))
