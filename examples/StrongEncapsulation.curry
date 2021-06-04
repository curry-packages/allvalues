------------------------------------------------------------------------------
--- Some example demonstrating the strong encapsulation property
--- of the operators in `Control.AllValues`. A detailed discussion
--- can be found in:
--- 
--- Bernd Braßel, Michael Hanus, Frank Huch:
--- Encapsulating Non-Determinism in Functional Logic Computations
--- J. Funct. Log. Program. 2004 (2004)
--- 
--- http://danae.uni-muenster.de/lehre/kuchen/JFLP/articles/2004/S04-01/A2004-06/JFLP-A2004-06.pdf
------------------------------------------------------------------------------

import Control.AllValues

-- The famous non-deterministic operation.
coin :: Int
coin = 0 ? 1

-- This result is obvious.
coinList1 :: [Int]
coinList1 = allValues coin ++ allValues coin
-- > [0,1,0,1]

-- This result shows the strong encapsulation view: although `coin`
-- is introduced outside `allValues`, both occurrences will be
-- independently encapsulated.
coinList2 :: [Int]
coinList2 = let x = coin in allValues x ++ allValues x
-- > [0,1,0,1]

-- However, if `coin` is evaluated before the encapsulation operators,
-- there is nothing more to encapsulated.
coinList3 :: [Int]
coinList3 = let x = coin in [x] ++ allValues x ++ allValues x
-- > [0,0,0] ? [1,1,1]

-- The first `allValues` encapsulates `coin` whereas it is evaluated
-- in the second occurrence of `allValues`.
coinList4 :: [Int]
coinList4 = let x = coin in allValues x ++ [x] ++ allValues x
-- > [0,1,0,0] ? [0,1,1,1]

-- Since `coin` is evaluated after the occurrences of `allValues`,
-- both occurrences evaluate the non-determinism of a copy of `coin`.
coinList5 :: [Int]
coinList5 = let x = coin in allValues x ++ allValues x ++ [x]
-- > [0,1,0,1,0] ? [0,1,0,1,1]

coinList6 :: [Int]
coinList6 = allValues coin ++ allValues coin ++ [coin]
-- > [0,1,0,1,0] ? [0,1,0,1,1]
