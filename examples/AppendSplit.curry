-- a simple example for encapsulated search:
import Control.AllValues (allSolutions)

append []     ys = ys
append (x:xs) ys = x : append xs ys

-- compute all solutions to equation  append [True] xs == [True,False]:
g1 = allSolutions (\xs -> append [True] xs == [True,False])

-- compute all solutions for xs in the equation  append _ xs == [True,False]:
g2 = allSolutions (\xs -> append _ xs == [True,False])

-- compute the list of all splittings of the list [True,False]:
g3 = allSolutions (\(l1,l2) -> append l1 l2 == [True,False])

