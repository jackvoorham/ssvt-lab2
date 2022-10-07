import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- The empty list is tested by anyList mutator which is able to output []. This makes this 
-- mutator strong because it generates lists of arbitrary length.
-- The addElements function if fairly strong since it adds elements to both ends, however it could be stronger
-- if it was also possible for it to only add on one end.
-- The removeElements is the same but in this case it should also remove all elements.
-- These mutators don't cover a non sorted list so one is made.
-- This mutator is covered by the properties.
-- These mutators also don't cover a reversed list.
-- This mutator is also covered by the properties.
-- Time spend: 80 minutes --

scrambleElements :: [Integer] -> Gen[Integer]
scrambleElements xs = do
    num <- choose (1, (length xs -1))
    let (f1, f2) = splitAt num xs
    return $ f2 ++ f1

reverseElements :: [Integer] -> Gen[Integer]
reverseElements xs = return $ reverse xs

