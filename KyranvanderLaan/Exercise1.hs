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

-- The actual assignment didn't take that long, it was the IO part that gave so much errors that it took a couple of hours.
-- The eventual test can be made better but for now this works.
-- Time spend: 4 hours.

testProbs :: [Float] -> (Float, Float, Float, Float) -> (Float, Float, Float, Float) 
testProbs [] (a,b,c,d) = (a,b,c,d)
testProbs (x:xs) (a, b, c, d) = if x < 0.25 && x > 0
                                    then testProbs (xs) (a+1,b,c,d)
                                    else if x < 0.5
                                        then testProbs (xs) (a,b+1,c,d)
                                        else if x < 0.75
                                            then testProbs (xs) (a,b,c+1,d)
                                            else testProbs (xs) (a,b,c,d+1)

testFloat :: (Float, Float, Float, Float) -> Bool
testFloat (a, b, c, d) = if a >= 2400 && a <= 2600 && b >= 2400 && b <= 2600 && c >= 2400 && c <= 2600 && d >= 2400 && d <= 2600
                            then True
                            else False

main :: IO Bool
main = do
    temp <- probs 10000
    return (testFloat (testProbs temp (0,0,0,0)))
