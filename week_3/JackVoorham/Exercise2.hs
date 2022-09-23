import Lecture3
import Test.QuickCheck
import FormGen

-- Checks the equality of the parsed string (we convert the form to string using show) and
-- check if the result is the same as the expected outcome after the parser 
equalityCheck :: Form -> Bool
equalityCheck form = [form] == parsedForm
                where
                    parsedForm = parse(show form)

-- Use the generator to create arbritraty forms, then pass them to the equalityCheck, if
-- the parser parses correctly we can pass the equalityChecks
main = do 
    quickCheck $ forAll genForm equalityCheck

-- Time spent: First version (3 hours), second (this) version (30 minutes)
-- Total: 3.5 hours

-- Test report:
-- +++ OK, passed 100 tests.