import Data.List
import LTS    
import Test.QuickCheck
import Exercise1

-- It took really long to figure out how to use the random string generator for the labels inside the ltsList function
-- The ltsList function creates a list of n (2 to 5) lts's to be used inside the IOLTS function.
-- Time spend: 300 minutes --

-- Generate labels for the transitions.
lableGen :: Gen String
lableGen = vectorOf 5 (elements ['a'..'z'])

getElement :: (Integer, [Char], Integer) -> [Char]
getElement (_, n, _) = n

-- generate a list with random labels without branches
ltsList :: Integer -> Gen [(Integer, [Char], Integer)]
ltsList counter = if counter == 0
    then do
        name <- lableGen
        let temp = (0, "?" ++ name, 1)
        return ([temp]) 
    else do
        temp <- (ltsList (counter -1))
        name <- lableGen
        let finallist = [(counter, "!" ++ name, counter+1)] ++ temp
        return (finallist)

-- generate a list with random labels with branches, makes sure there are no "?" transitions after "!"
ltsList' :: Integer -> Gen [(Integer, [Char], Integer)]
ltsList' counter = if counter == 0
    then do
        name <- lableGen
        let temp = (0, "?" ++ name, 1)
        return ([temp]) 
    else do
        temp <- (ltsList' (counter -1))
        name <- lableGen
        if head (getElement (head temp)) == '?'
            then do
                choser <- choose(0, 1) :: Gen Int
                let finalist = case choser of
                            0 -> [(counter-1, "?" ++ name, counter+1)] ++ temp
                            1 -> [(counter, "!" ++ name, counter+1)] ++ temp
                return (finalist)
            else do
                let finalist = [(counter, "!" ++ name, counter+1)] ++ temp
                return (finalist) 

-- generate between two and five states
ltsGen :: Gen IOLTS
ltsGen = do
    states <- choose(1, 5) :: Gen Integer
    temp <- ltsList states
    let temp' = createIOLTS temp
    return temp'

-- generate between two and five states including branches
ltsGen' :: Gen IOLTS
ltsGen' = do
    states <- choose(1, 5) :: Gen Integer
    temp <- ltsList states
    let temp' = createIOLTS temp
    return temp'

testProp :: IOLTS -> Bool
testProp n = validateLTS n == True

testProp' :: IOLTS -> Bool
testProp' n = validateLTS n == True

main :: IO()
main = do
    quickCheck $ forAll ltsGen testProp
    quickCheck $ forAll ltsGen' testProp'