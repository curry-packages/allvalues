-- Module SportsDB from Escher report

import Control.AllValues ( allValues )

allSolutions :: Data a => (a -> Bool) -> [a]
allSolutions p = allValues (invertPred p)

-- Inverts a predicate, i.e., compute all values for which the predicate
-- succeeds.
invertPred :: Data a => (a -> Bool) -> a
invertPred p | p x = x where x free

-----------------------------------------------------------------------------

data Person = Mary | Bill | Joe | Fred

data Sport = Cricket | Football | Tennis
 deriving Eq

likes :: Person -> Sport -> Bool
likes Mary Cricket  = True
likes Mary Tennis   = True
likes Bill Cricket  = True
likes Bill Tennis   = True
likes Joe  Tennis   = True
likes Joe  Football = True


q1 s = likes Fred s   --> no solution

-- implementation of Escher's forall-construct:
forall :: Data a => (a -> Bool) -> (a -> Bool) -> Bool
forall domain cond = all cond (allSolutions domain)

q2 x = forall (\y-> y `elem` [Cricket,Tennis])
              (\y-> likes x y)

--> {x=Mary} | {x=Bill}
