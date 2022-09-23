import Lecture3
import Test.QuickCheck
import SetOrd
import FormGen -- Change to import Exercise4

-- Notes for myself:
-- A subformula is subset of the formula, the sub function finds them all
-- First intiution, the set of all subformola of a formula f also contains the formula f itself

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

res = all subPropertyOne [form1, form2, form3, form4]

main = do
    verboseCheck $ forAll genForm subPropertyOne