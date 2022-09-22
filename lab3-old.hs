module Lab3 where
import Data.List
import Data.Char
import Data.Maybe
import Text.Printf
import Lecture3

type Map k v = [(k, v)]
type TruthValues = Map Name Bool
type Name = Int
data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)

instance Show Form where
    show (Prop x)   = show x
    show (Neg f)    = '-' : show f
    show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
    show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
    show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
    show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"

lookupRequire :: (Eq k, Show k) => k -> Map k v -> v
lookupRequire k m = case lookup k m of
    Just v -> v
    Nothing -> error (printf "Key '%s' was not found in map." (show k))

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\acc curr -> if curr `elem` acc then acc else acc ++ [curr]) []

uniqueNames :: Form -> [Name]
uniqueNames f = case f of
    Prop n -> [n]
    Neg f' -> uniqueNames f'
    Cnj fs -> uniqueNamesFromElements fs
    Dsj fs -> uniqueNamesFromElements fs
    Impl f1 f2 -> uniqueNamesFromElements [f1, f2]
    Equiv f1 f2 -> uniqueNamesFromElements [f1, f2]
    where uniqueNamesFromElements fs' = do result <- removeDuplicates $ map (\f' -> uniqueNames f') fs'; result

isTrue :: TruthValues -> Form -> Bool
isTrue t f = case f of
    Prop n -> lookupRequire n t
    Neg f' -> isFalse' f'
    Cnj fs -> all isTrue' fs
    Dsj fs -> any isTrue' fs
    Impl f1 f2 -> isFalse' f1 || isTrue' f2
    Equiv f1 f2 -> (isTrue' f1 && isTrue' f2) || (isFalse' f1 && isFalse' f2)
    where isTrue' = isTrue t
          isFalse' = isFalse t

isFalse :: TruthValues -> Form -> Bool
isFalse t f = not (isTrue t f)

form1 :: Form
form1 = Impl (Prop 33) (Prop 34)

