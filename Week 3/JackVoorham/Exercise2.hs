import Lecture3
import Test.QuickCheck

-- TODO: Comments

--------------------------------------------

-- Generation functions

returnProp :: Int -> (String, Form)
returnProp x = (show x, Prop x) 

returnNeg :: Int -> (String, Form)
returnNeg x = ("-" ++ show x, Neg (Prop x))

returnCnjProp :: Int -> (String, Form)
returnCnjProp x = ("*(" ++ p ++ ")", Cnj q) where
    p = fst(returnProp x)
    q = [snd(returnProp x)]
    
returnCnjNeg :: Int -> (String, Form)
returnCnjNeg x = ("*(" ++ p ++ ")", Cnj q) where
    p = fst(returnNeg x)
    q = [snd(returnNeg x)]
    
returnImplProp :: Int -> (String, Form)
returnImplProp x = ("(" ++ p ++ "==>" ++ p ++ ")", Impl q q) where
    p = fst(returnProp x)
    q = snd(returnProp x)

returnImplNeg:: Int -> (String, Form)
returnImplNeg x = ("(" ++ p ++ "==>" ++ p ++ ")", Impl q q) where
    p = fst(returnNeg x)
    q = snd(returnNeg x)

returnEquivProp :: Int -> (String, Form)
returnEquivProp x = ("(" ++ p ++ "<=>" ++ p ++ ")", Equiv q q) where
    p = fst(returnProp x)
    q = snd(returnProp x)

returnEquivNeg:: Int -> (String, Form)
returnEquivNeg x = ("(" ++ p ++ "<=>" ++ p ++ ")", Equiv q q) where
    p = fst(returnNeg x)
    q = snd(returnNeg x)

------------------------------------------

-- Testing functions

equalityCheck :: (String, Form) -> Bool
equalityCheck tup = parse (fst tup) == [snd tup]

propTest :: Int -> Bool
propTest x = equalityCheck (returnProp x)

negTest :: Int -> Bool
negTest x = equalityCheck (returnProp x)

cnjTestOne :: Int -> Bool
cnjTestOne x = equalityCheck (returnCnjProp x)

cnjTestTwo :: Int -> Bool
cnjTestTwo x = equalityCheck (returnCnjNeg x)

implTestOne :: Int -> Bool
implTestOne x = equalityCheck (returnImplProp x)

implTestTwo :: Int -> Bool
implTestTwo x = equalityCheck (returnImplNeg x)

equivTestOne :: Int -> Bool
equivTestOne x = equalityCheck (returnEquivProp x)

equivTestTwo :: Int -> Bool
equivTestTwo x = equalityCheck (returnEquivNeg x)
-------------------------------------------
genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

main = do 
    putStrLn "== Testing Form with Prop ==" 
    quickCheck $ forAll genPos propTest
    putStrLn "== Testing Form with Neg ==" 
    quickCheck $ forAll genPos negTest
    putStrLn "== Test Form with Cnj with Prop ==" 
    quickCheck $ forAll genPos cnjTestOne 
    putStrLn "== Test Form with Cnj with Neg ==" 
    quickCheck $ forAll genPos cnjTestTwo 
    putStrLn "== Test Form with Impl with Prop ==" 
    quickCheck $ forAll genPos implTestOne 
    putStrLn "== Test Form with Impl with Neg ==" 
    quickCheck $ forAll genPos implTestTwo 
    putStrLn "== Test Form with Equiv with Prop ==" 
    quickCheck $ forAll genPos equivTestOne 
    putStrLn "== Test Form with Equiv with Neg ==" 
    quickCheck $ forAll genPos equivTestTwo 