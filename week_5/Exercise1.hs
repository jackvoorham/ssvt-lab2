import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- The two given mutators add elements to the list and remove elements from the list.
-- The empty output list isn't tested by these because remove elements doesn't remove all elements.
-- It also doesn't mutate to a non sorted list (reversed).

emptyList :: [Integer] -> Gen [Integer]
emptyList xs = return []

scrambleElements :: [Integer] -> Gen[Integer]
scrambleElements xs = return $ reverse xs
