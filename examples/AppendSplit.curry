-- a simple example for encapsulated search:
import Control.AllValues

append []     ys = ys
append (x:xs) ys = x : append xs ys

-- compute all solutions to equation  append [True] xs =:= [True,False]:
appTrue | append [True] xs =:= [True,False] = xs  where xs free

g1 = allValues appTrue

-- compute one solution to equation  append [True] xs =:= [True,False]:
g2 = oneValue appTrue

-- compute the list of all splittings of the list [True,False]:
g3 = allValues splitList

splitList | append l1 l2 =:= [True,False] = (l1,l2)  where l1,l2 free

