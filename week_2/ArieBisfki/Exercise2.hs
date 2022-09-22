module Exercise2 where
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

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Helper function which takes a triplet of arguments and creates a list of 3 tuples which together represent
-- all the unique ordererings of the triplet arguments.
combinations :: Int -> Int -> Int -> [(Int, Int, Int)]
combinations n m k = [(n, m, k), (n, k, m), (m, k, n)]

-- Generates positive integer
-- https://stackoverflow.com/a/39292322
genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

-- Generates triplet which are not triangle sides
genNoTriangleTriplet :: Gen (Int, Int, Int)
genNoTriangleTriplet = (arbitrary :: Gen (Int, Int, Int)) `suchThat` (\(n,m,k) -> all (\(x,y,z) -> x + y <= z) $ combinations n m k )

genIsoscelesTriplet :: Gen (Int, Int, Int)
genIsoscelesTriplet = do
    n <- genPos
    let m = n
    k <- genPos `suchThat` (/= n)
    return (n, m, k)

genOtherTriplet :: Gen (Int, Int, Int)
genOtherTriplet = do
    n <- genPos
    m <- genPos `suchThat` (/= n)
    k <- genPos `suchThat` (\x -> x /= n && x /= m && all (\(a, b, c) -> a^2 + b^2 /= c^2) (combinations n m x))
    return (n, m, k)

-- Generates pythagorean triplet (of positives)
-- https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples
--genPythagoreanTriplet :: Gen (Int, Int, Int)

triangle :: Int -> Int -> Int -> Shape
triangle n m k
    -- Two sides must be greater than the third. This must be the case for all combinations. Otherwise it's not a triangle.
    | not $ any (\(x, y, z) -> x + y > z) (combinations n m k) = NoTriangle
    -- If all 3 sides are equal, then the triangle is Equilateral. Note that we do not need to additionally check n == k here due to transitivity.
    | n == m && m == k = Equilateral
    -- If the triplet is a pythagorean triplet, then the triangle is Rectangular.
    | any (\(x, y, z) -> x^2 + y^2 == z^2) (combinations n m k) = Rectangular
    -- If 2 sides are equal, then the triangle is Isosceles.
    | n == m || m == k || n == k = Isosceles
    -- Triangle must be other.
    | otherwise = Other

-- I had to first read up on haskell datatypes.
-- Then I had to first find the rules for each type of triangle.
-- Afterwards it was quite simple to implement each rule in the correct order.
-- Time spent: 1h.
-- But then I had to write test cases...
-- It was difficult to write a generator for the pythagorean triplet.
-- Time spent on writing tests: 2h.
main :: IO()
main = do
    putStrLn "== Equilateral ==";
    quickCheck $ forAll genPos $ \x -> (triangle x x x) == Equilateral;
    putStrLn "== NoTriangle ==";
    quickCheck $ forAll genNoTriangleTriplet $ \(x, y, z) -> (triangle x y z) == NoTriangle;
    --putStrLn "== Rectangular ==";
    --quickCheck $ forAll genRectangularTriplet $ \(x, y, z) -> (triangle x y z) == Rectangular;
    putStrLn "== Isosceles ==";
    quickCheck $ forAll genIsoscelesTriplet $ \(x, y, z) -> (triangle x y z) == Isosceles;
    putStrLn "== Other ==";
    quickCheck $ forAll genOtherTriplet $ \(x, y, z) -> (triangle x y z) == Other;

