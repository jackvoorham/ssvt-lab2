module Exercise6 where
import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

-- I could not figure out how the notation was supposed to look with a formula that was more than 2 levels deep. Because the typing is strictly defined
-- to have only 2 levels. I spent my head butting against the exercise for a while until I gave up.

-- One of the things I tried
-- was defining a data type which has 3 different constructors, one which accepts an int, one which accepts an [int],
-- and one which accepts an [[int]]. So respectively for Prop and Neg, Dsj, Cnj but in the end it turned out as nonsense.

-- Time spent: 150 minutes --
