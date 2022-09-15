module Exercise2 where
import Data.List
import Data.Char
import System.Random

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Helper function which takes a triplet of arguments and creates a list of 3 tuples which together represent
-- all the unique ordererings of the triplet arguments.
combinations :: Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
combinations n m k = [(n, m, k), (n, k, m), (m, k, n)]

-- I had to first read up on haskell datatypes.
-- Then I had to first find the rules for each type of triangle.
-- Afterwards it was quite simple to implement each rule in the correct order.
-- Time spent: 1h.
triangle :: Integer -> Integer -> Integer -> Shape
triangle n m k
    -- Two sides must be greater than the third. This must be the case for all combinations. Otherwise it's not a triangle.
    | not $ any (\(x, y, z) -> x + y == z) (combinations n m k) = NoTriangle
    -- If all 3 sides are equal, then the triangle is Equilateral. Note that we do not need to additionally check n == k here due to transitivity.
    | n == m && m == k = Equilateral
    -- If the triplet is a pythagorean triplet, then the triangle is Rectangular.
    | any (\(x, y, z) -> x^2 + y^2 == z^2) (combinations n m k) = Rectangular
    -- If 2 sides are equal, then the triangle is Isosceles.
    | n == m || m == k || n == k = Isosceles
    -- Triangle must be other.
    | otherwise = Other

