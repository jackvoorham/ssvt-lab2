module Exercise1 (setGen, setRandom) where
import Test.QuickCheck
import System.Random
import SetOrd

-- I lost some time trying to figure out what was meant by the assignment description. In my opinion it was not
-- clear what the two generators were supposed to be. I only kinda found out by asking around. Most people
-- were confused about it as well.
-- Anyway I first implemented the QuickCheck version. I spent some time there on making the instance of the
-- Arbitrary class. It took a while before I realised that I had to put the Arbitrary constraint
-- on the items of the generated set. This source helped me realised that: https://stackoverflow.com/questions/45592769/instance-of-arbitrary-for-custom-list-like-type
-- I then implemented the System.Random version. This was straightforward. I only had to find out how to
-- randomly generate a generic value. randomIO was the function for this in the end.
-- Time spent: 150m

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = setGen

setGen :: (Arbitrary a, Ord a) => Gen (Set a)
setGen = do
    list <- arbitrary
    return $ list2set list

setRandom :: (Random a, Ord a) => IO (Set a)
setRandom = do
  length <- randomRIO (0, 20)
  setRandom' length

setRandom' :: (Random a, Ord a) => Int -> IO (Set a)
setRandom' 0 = return emptySet
setRandom' n = do
  set <- setRandom' $ n - 1
  newItem <- randomIO
  return $ insertSet newItem set
