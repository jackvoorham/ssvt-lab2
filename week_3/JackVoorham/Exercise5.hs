import Lecture3
import Test.QuickCheck
import SetOrd
import FormGen -- Change to import Exercise4

-- Notes for myself:
-- A subformula is subset of the formula, the sub function finds them all
-- First intiution, the set of all subformola of a formula f also contains the formula f itself
-- Second, all sub's of the entries in sub are also in sub, that is, if f is a subset of sub and f' is a subset of the sub of f then f' is also in the sub of f

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

subPropertyOne :: Form -> Bool
subPropertyOne x = inSet x subForms where
    subForms = sub x

-- Removes duplicates from a list
-- Retrieved from: https://stackoverflow.com/a/31151043
myNub :: (Eq a) => [a] -> [a]
myNub (x:xs) = x : myNub (filter (/= x) xs)
myNub [] = []

-- We can simply for nsub change the original sub to make a list. We can simply use list concatenation
-- instead of the unionSet call 
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
subn :: Form -> Int
subn f = length $ myNub (subList f)

-- Very trivial test, i dont know how to test it more rigorously...
checkSubn :: String
checkSubn | all (\x -> subn x == 7) [form1, form2] = "+++ Passed subn test."
          | otherwise = "+++ Did not pass subn test." 
        
main = do
    putStrLn checkSubn
    quickCheck $ forAll genForm subPropertyOne
