import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- The program works by doing 10000 tests on a set of properties given a certain mutator.
-- We use mutate' to check if all mutators are killed. The and checks if everything is True.
-- If this is the case, no properties in the set killed the mutator.
-- The overall percentage is calculated by seeing how many mutators were killed.
-- Time spend: 200 minutes --

-- I used the following answer from stackoverflow to figure out how to get a float
-- from a division with two integers:
-- https://stackoverflow.com/questions/3275193/whats-the-right-way-to-divide-two-int-values-to-obtain-a-float
strength :: [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> Integer -> Integer -> Gen Float
strength _ _ 0 percentage = return $ (fromIntegral(10000 - percentage)) / (fromIntegral(100)) 
strength xs mut count percentage = do
    mutationResult <- mutate' mut xs multiplicationTable 2 
    let newPercentage = if and mutationResult then percentage + 1 else percentage
    strength xs mut (count - 1) newPercentage

calculateStrength :: [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> Gen Float
calculateStrength xs mut = (strength xs mut 10000 0)

testStrength :: [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> IO ()
testStrength xs mut = do 
    temp <- generate $ calculateStrength xs mut
    putStr ("Mutators killed: " ++ (show temp) ++ "%\n")