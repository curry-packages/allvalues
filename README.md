allvalues: Encapsulate non-deterministic computations
=====================================================

This package contains the library `Control.AllValues`
which defines operations to encapsulate non-deterministic computations
so that they can be embedded in purely functional computations,
e.g., in I/O computations. The operations returns all values
of an expression in a list structure or a single value in a `Maybe`
container.

The Curry implementations PAKCS and KiCS2 use an incomplete
depth-first search strategy to encapsulate non-determinism,
but KiCS2 provides more operators to select the search strategy
(see package `searchtree`).
The Curry implementation Curry2Go uses a complete (parallel) search strategy.

The library implements **strong encapsulation** as discussed in
[this paper](http://www.informatik.uni-kiel.de/~mh/papers/JFLP04_findall.html).

--------------------------------------------------------------------------
