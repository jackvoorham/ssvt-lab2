module Lab3 where
import Data.List
import Data.Char
import Data.Maybe
import Text.Printf
import Lecture3

-- I had to first give myself a refresher on the definitions of the 4 terms. So I looked up materials.
-- I then got to work but didn't immediately see the Lecture3.hs file so I unfortunately set out to
-- reinvent the wheel. I had already implemented the equivalent of the 'propNames' function and the
-- 'evl' function when I finally found out about the Lecture3.hs file.
-- By this time I had spent around 5h or so but with the Lecture3 module I was able to implement
-- the actual functions in an hour.
-- Time spent: 360 minutes --

contradiction :: Form -> Bool
contradiction f = all (\vs -> not $ evl vs f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\vs -> evl vs f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\vs -> evl vs $ Impl f1 f2) (allVals $ Cnj [f1, f2])

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\vs -> (evl vs f1) == (evl vs f2) ) (allVals $ Cnj [f1, f2])
