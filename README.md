# loco

A Constraint Programming library. This is a Clojure wrapper of the Java library "Choco".
This library is the rebirth of "CloCoP", a wrapper of JaCoP, and since then I've switched the
underlying Java library (and made some general improvements to my API for CP in Clojure).

The name "Constraint Programming" represents a set of problems that can be expressed by a series of
constraints. What's interesting about these problems is that solutions are very easy to check, but not
very easy to find. For example, if I have the constraint "(X * 3) mod (Y ^ 5) = 4", it's very easy
to verify that this fact holds (given an X and a Y), but not nearly as easy to come up with an X and a Y that
satisfy the rule. The idea of Constraint Programming, however, is to find a way to come up with these solutions,
presumably much faster than brute force.

#### But what about core.logic?

You may start to notice a suspicious similarity to core.logic. I agree that both serve as finite-domain solvers,
but in my opinion, loco is a better choice for solving finite-domain problems:
- loco has a better suite of constraints (including arithmetic, logic, and global constraints) tailored to
more challenging problems.
- loco is significantly faster at solving most problems, because it has a more low-level finite-domain engine
underneath.


## Usage

(Insert lein dependency here)

Here is a sample problem written in Loco:

	(defn sample-problem
	  []
	  (let [s (solver "sample_problem")
	        x (int-var s "x" 1 5)
	        y (int-var s "y" 2 6)]
	    (constrain! s
	      ($= ($+ x y) 10)
	      ($= x y))
	    (solution s)))
	=>
	{"x" 5, "y" 5}

## API

#### The solver

The "solver" is essentially a container for all of the variables and constraints in your CP problem.
Calls to <code>int-var</code> and <code>constrain!</code> require that the corresponding solver be the first argument.
You can create a solver using <code>loco.core/solver</code>:

	(solver "mySolver")
	(solver)

#### Creating vars

In Loco, the only type of variable is an integer variable, or "int-var" for short.
Every int-var has its own domain, which is a finite set of integers.
The default type of int-var (an enumerated int-var) keeps track of its domain using a Bit Set.
However, a "bounded int-var" is a variation on this concept, which keeps track of only a minimum and a maximum of
the domain. It provides a trade-off between memory and running time (decreasing memory usage but increasing running time).
I recommend sticking with the enumerated variables unless you are running low on memory.

To create an int-var, use the <code>loco.core/int-var</code> function:

	(int-var solver name? min max)
	(int-var solver name? values)
	(int-var solver name? min max :bounded)
	Examples:
	(def s (solver))
	(int-var s "a" 1 5)
	(def b (int-var s 1 5))
	(int-var s "c" [1 2 3 4 5])
	(int-var s "d" 1 5 :bounded)

In all cases, the variable name is optional. If you provide a name however, you get the following two benefits:
- Inside constraints, you can refer to the variable by its name rather than passing in the variable itself.
- In a solution map returned from <code>(solution solver)</code>, only named variables will appear.

#### Creating constraints

All constraints are in the <code>loco.constraints</code> namespace. Also, the functions that generate the constraints
all begin with <code>$</code> (e.g. <code>$=</code> for "=", <code>$all-different?</code> for "all different", etc.) so
that you can call <code>(use 'loco.constraints)</code> without overlap with clojure.core functions.

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
