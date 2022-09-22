module Exercise4 where
import Data.List
import Data.Char
import Data.Maybe
import Text.Printf
import Lecture3
import Lab3
import Test.QuickCheck

genForm :: Gen Form
genForm = do
    depth <- choose(0, 10) :: Gen Int
    genForm' depth

genForm' :: Int -> Gen Form
genForm' depth = if depth == 0
    then genProp
    else do
        form <- genForm' $ depth - 1
        fType <- choose(0, 2) :: Gen Int
        case fType of
            0 -> genNeg $ depth - 1
            1 -> genCnj $ depth - 1
            2 -> genDsj $ depth - 1
            3 -> genImpl $ depth - 1
            4 -> genEquiv $ depth - 1

genProp :: Gen Form
genProp = do
    name <- choose(0, 4)
    return $ Prop name

genNeg :: Int -> Gen Form
genNeg depth = do
    form <- genForm' depth
    return $ Neg form

genCnj :: Int -> Gen Form
genCnj depth = do
    let width = 2
    forms <- genForms depth width
    return $ Cnj $ forms

genDsj :: Int -> Gen Form
genDsj depth = do
    let width = 2
    forms <- genForms depth width
    return $ Dsj $ forms

genImpl :: Int -> Gen Form
genImpl depth = do
    form1 <- genForm' depth
    form2 <- genForm' depth
    return $ Impl form1 form2

genEquiv :: Int -> Gen Form
genEquiv depth = do
    form1 <- genForm' depth
    form2 <- genForm' depth
    return $ Equiv form1 form2

genForms :: Int -> Int -> Gen [Form]
genForms depth amount = sequence $ map (\i -> genForm' depth) [0..amount]
