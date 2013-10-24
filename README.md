# loco

A Constraint Programming library. This is a Clojure wrapper of the Java library "Choco".
This library is the rebirth of "CloCoP", a wrapper of JaCoP, and since then I've switched the
underlying Java library (and made some general improvements to my API for CP in Clojure).

#### But what about core.logic?

You may start to notice a suspicious similarity to core.logic. I agree that both serve as finite-domain solvers,
but here are a few ways that, in my opinion, make loco a better choice for pure finite-domain problems:
- loco has a better suite of constraints (including arithmetic, logic, and global constraints) tailored to
more challenging problems.
- loco is significantly faster at solving most problems, because it has a more low-level finite-domain engine
underneath.


## Usage

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
