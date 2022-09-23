module Exercise1 where
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
-- As for testing, I first wrote down the 3 forms from the Lecture3 module in a more readable notation:
-- 1. (p -> q) <-> (!q -> !p)
-- 2. (p -> q) <-> (!p -> !q)
-- 3. ((p -> q) ∧ (q -> r)) -> (p -> r)
-- I drew truth tables on a piece of paper in order to know what to expect for the rules.
-- The first rule I recognized as modus tollens, which is a tautology.
-- The second rule I recognized as denying the antecedent, which is false for p = F and q = T.
-- The third rule I recognized as transitivity, which is a tautology.

contradiction :: Form -> Bool
contradiction f = all (\vs -> not $ evl vs f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\vs -> evl vs f) (allVals f)

-- For getting all vals, create a conjunction between f1 and f2 and then call allVals on that
entails :: Form -> Form -> Bool
entails f1 f2 = all (\vs -> evl vs $ Impl f1 f2) (allVals $ Cnj [f1, f2])

-- For getting all vals, create a conjunction between f1 and f2 and then call allVals on that
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\vs -> (evl vs f1) == (evl vs f2) ) (allVals $ Cnj [f1, f2])

-------------------------------------------------------------------------
    
-- Time spent: 360 minutes --
