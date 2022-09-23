module Exercise2 where
import Lecture3
import Test.QuickCheck
import Exercise4

-- Checks the equality of the parsed string (we convert the form to string using show) and
-- check if the result is the same as the expected outcome after the parser call
equalityCheck :: Form -> Bool
equalityCheck form = [form] == parsedForm
                where
                    parsedForm = parse(show form)

-- Use the generator to create arbritraty forms, then pass them to the equalityCheck, if
-- the parser parses correctly we can pass the equalityChecks
mainTwo = do 
    quickCheck $ forAll genForm equalityCheck
    
-------------------------------------------------------------------------

-- Time spent: First version (3 hours), second (this) version (30 minutes)
-- Time spent: 210 minutes --

-- Test report (running mainTwo):
-- +++ OK, passed 100 tests.