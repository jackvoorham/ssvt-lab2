import Data.List
import LTS    
import Test.QuickCheck

-- 

-- Generate labels for the transitions.
lableGen :: Gen String
lableGen = vectorOf 5 (elements ['a'..'z'])

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

-- generate between 2 and five states
ltsGen :: Gen IOLTS
ltsGen = do
    states <- choose(1, 5) :: Gen Integer
    temp <- ltsList states
    let temp' = createIOLTS temp
    return temp'

