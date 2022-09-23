import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- The CNF form has no arrows hence we can use the arrowfree function form
-- the given lecture3.hs file. This function is tested using the examples from
-- the lecture (namely p->q and p<->p) which convert to the correct output also
-- given in the Lecture.
-- I used the following website for a clear overview of the steps needed to 
-- convert a form to cnf: http://www.mathcs.duq.edu/simon/Fall04/notes-6-20/node2.html
-- nnf can be used to convert to negation normal form.
-- Time spend: 350 minutes --

-- Start the formula and determine if you need to use de morgan's law
-- It took a rather long time to figure out that each possible instance
-- of the form needs to be declared individually else the Cnj and Dsj return
-- non exhaustive search patterns
firstForm :: Form -> Form
firstForm (Prop x) = Prop x
firstForm (Neg x) = Neg x
-- The inspiration for this declaration was taken from the
-- nnf and arrowfree functions since Cnj doesn't need to be changed
firstForm (Cnj x) = Cnj (map firstForm x)
-- Dsj needs to be changed with demorgan's law but only if it consists
-- of multiple forms else it can stay that way
firstForm (Dsj [x]) = firstForm x
firstForm (Dsj (x:xs)) = deMorg x (firstForm (Dsj xs))

deMorg :: Form -> Form -> Form
-- If you have one element in the conjecture, it needs to become only 
-- a distjuncture of the two elements (otherwise you get half empty
-- conjectures in your end formula)
deMorg (Cnj [x]) xs = deMorg x xs
deMorg x (Cnj [xs]) = deMorg x xs
-- If you have a conjecture consisting of two or more elements on one side
-- you need to put the other half of the disjuncture over all elements
deMorg (Cnj (y:ys)) xs = Cnj [(deMorg y xs),(deMorg (Cnj ys) xs)]
deMorg x (Cnj (y:ys)) = Cnj [(deMorg x y),(deMorg x (Cnj ys))]
deMorg x xs = Dsj [x, xs]

cnf :: Form -> Form
cnf f = firstForm (nnf (arrowfree f))