module Exercise5 where
import Lecture3
import Test.QuickCheck
import SetOrd
import Exercise4 

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- Property one of sub; The formula f is a subset of the subformulas of f
subPropertyOne :: Form -> Bool
subPropertyOne x = inSet x subForms where
    subForms = sub x
    
-- Removes duplicates from a list
-- Retrieved from: https://stackoverflow.com/a/31151043
myNub :: (Eq a) => [a] -> [a]
myNub (x:xs) = x : myNub (filter (/= x) xs)
myNub [] = []

-- We can simply change the original sub to make a list. We can simply use list concatenation
-- instead of the unionSet call, we call this function in nSub because length is defined on 
-- lists
subList :: Form -> [Form]
subList (Prop x) = [Prop x]
subList (Neg f) = Neg f : subList f
subList f@(Cnj [f1,f2]) = (f : subList f1) ++ subList f2
subList f@(Dsj [f1,f2]) = (f : subList f1) ++ subList f2
subList f@(Impl f1 f2) = (f : subList f1) ++ subList f2
subList f@(Equiv f1 f2) = (f : subList f1) ++ subList f2

-- Takes a form, calls the subList function which creates a list from the subformulas
-- make entries unique in that list, the length of this list is then the number of
-- subformula's
nsub :: Form -> Int
nsub f = length $ myNub(subList f)

-- Very trivial test, i dont know how to test it more rigorously...
-- Just test two of the given forms (that are both of length 7) and see if they match
checkNsub :: String
checkNsub | all (\x -> nsub x == 7) [form1, form2] = "+++ Passed nsub test."
          | otherwise = "+++ Did not pass nsub test." 
        
main = do
    putStrLn checkNsub
    putStrLn "Testing subset property for sub..."
    quickCheck $ forAll genForm subPropertyOne
    
-------------------------------------------------------------------------

-- Time spent: 2 hours 5.1, mostly trying to figure out a second property, 1 hours 5.1
-- Time spent: 180 minutes --

-- 5.1 first proprety: Check if the formula f is a subset of the subformula's of f, this must always
-- be the case as the f is always a subformula of itself.

-- Test report (running main):
-- +++ Passed nsub test.
-- Testing subset property for sub...
-- +++ OK, passed 100 tests.