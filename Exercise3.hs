module Exercise3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Base code imported from Lab 2 at https://canvas.uva.nl/courses/32483/files/7147684?wrap=1&fd_cookie_set=1

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- stronger weaker function was taken from the exercise.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

even' :: Int -> Bool
even' x = x `mod` 2 == 0

prop1 :: Int -> Bool
prop1 x = even' x && x > 3

prop2 :: Int -> Bool
prop2 x = even' x || x > 3

prop3 :: Int -> Bool
prop3 x = (even' x && x > 3) || even' x

funcName :: String -> (Int -> Bool)
funcName f
    | f == "even'" = even'
    | f == "prop1" = prop1
    | f == "prop2" = prop2
    | f == "prop3" = prop3

insertElem :: [String] -> String -> [String]
insertElem [] f = [f]
insertElem (x:xs) f
    | stronger [-10..10] (funcName f) (funcName x) == True = f:x:xs
    | otherwise = [x] ++ insertElem xs f

main :: IO()
main = do
    let list1 = insertElem [] "even'"
    let list2 = insertElem list1 "prop1"
    let list3 = insertElem list2 "prop2"
    let list4 = insertElem list3 "prop3"
    print (list4)

-----------------------------

-- the properties themselves weren't hard too implement.
-- the descending list took a bit longer since we couldn't figure out how to print a function
-- and after that the IO again gave a lot of problems with us just wanting to call the insertElem function from main (which we couldn't).
-- We fixed this by creating multiple let's (surely can be done more clean but we don't know how).
-- The output of the program gives: ["prop1","prop3","even'","prop2"], which means prop1 is the strongest and prop2 the weakest in the range [-10..10]
-- Time spend: 2 hours
