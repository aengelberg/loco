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

	[loco "0.1.0-SNAPSHOT"]

Here is a sample problem written in Loco:

	(defn sample-problem
	  []
	  (solution
	    [($int :x 1 5)
	     ($int :y 2 6)
	     ($= ($+ :x :y) 10)
	     ($= :x :y)]))
	=>
	{"x" 5, "y" 5}

## API

A "problem" is a list of constraints. These constraints can either be the creation of integer variables,
or constraints on already created integer variables.

#### Creating vars

In Loco, the only type of variable is an integer variable, or "int-var" for short.
Every int-var has its own domain, which is a finite set of integers.
The default type of int-var (an enumerated int-var) keeps track of its domain using a Bit Set.
However, a "bounded int-var" is a variation on this concept, which keeps track of only a minimum and a maximum of
the domain. It provides a trade-off between memory and running time (decreasing memory usage but increasing running time).
I recommend sticking with the enumerated variables unless you are running low on memory.

Creating an int-var occurs in the form of a constraint, <code>loco.constraints/$int</code>.
Exactly one <code>$int</code> statement is required for each var that is referenced to in the other constraints.

	($int name min max)
	($int name values)
	($int name min max :bounded)
	Examples:
	($int :a 1 5)
	($int :b [1 2 3 4 5])
	($int :c 1 5 :bounded)
	(solution [($int :x 1 5)
	           ($= :x 2)])
	; declarations of vars can appear after a constraint that references it
	(solution [($= :x 2)
	           ($int :x 1 5)])

A "boolean var" (or "bool-var") is an int-var that has the domain #{0 1}. You can create one with

	($bool name)

#### Variable names

In Loco, there are two valid types that can be used as a variable name. Keywords, and vectors that begin with keywords.
For example:

	($int :a 1 5)
	($int :a_b 1 5)
	($int [:a 1] 1 5)
	($int [:b [1 2] {1 5} "a"] 1 5)  ; still valid
	($int [1 2] 1 5) ; not valid

The reason behind this design decision is that a lot of formal notation for CP problems uses subscripts
to represent an array of variables that are related enough to have the same name but be distinguished by
an id number. (LaTeX image below)

<img src="http://latex.codecogs.com/gif.latex?\newline%20x_1%20\times%20y%20=%20z%20\newline%20x_2%20+%20z_3%20=%202%20\newline%20\text{alldifferent}(x_{1,1},%20x_{1,2}%20...%20x_{1,9})" border="0"/>

Here are the Loco equivalents of the constraints shown above:

	($= ($* [:x 1] :y) :z)
	($= ($+ [:x 2] [:z 3]) 2)
	(apply $all-different? (for [j (range 1 (inc 9))]
	                         [:x 1 j]))


#### Creating constraints

All constraints are in the <code>loco.constraints</code> namespace. Also, the functions that generate the constraints
all begin with <code>$</code> (e.g. <code>$=</code> for "=", <code>$all-different?</code> for "all different", etc.) so
that you can safely call <code>(use 'loco.constraints)</code> without overlap with clojure.core functions.

Note that some of these functions don't actually return
constraints, but will generate new variables to be used in constraints. For example, you'd never want to
constrain <code>($+ x y)</code>, but you might constrain <code>($= ($+ x y) 2)</code>.

Here is a complete list of all of the constraints available to you.
- <code>$+</code> - given a mixture of variables / numbers, returns the sum.
- <code>$-</code> - given a mixture of variables / numbers, returns <code>X - Y - Z - ...</code>,
or <code>-X</code> if only one argument is given.
- <code>$*</code> - given two arguments (one of which is allowed to be a number >= -1), returns the product.
- <code>$min</code> - returns the minimum of several arguments.
- <code>$max</code> - returns the maximum of several arguments.
- <code>$mod</code> - given two arguments X and Y, returns X mod Y.
- <code>$scalar</code> - given a list of variables (X, Y, Z, ...) and a list of integer coefficients (a, b, c, ...)
returns <code>aX + bY + cZ + ...</code>.

- <code>$=, $<, $>, $<=, $>=, $!=</code> - constraints that specify equality/inequality between two or more arguments.
Calling these on more than one argument will return a composition of multiple constraints (which has the
same functionality, but might be less efficient then you'd like).

- <code>$and</code> - given zero or more constraints, returns another constraint that is the "and" of the subconstraints,
i.e. it is true iff all of the subconstraints is true.
- <code>$or</code> - given zero or more constraints, returns another constraint that is the "or" of the subconstraints,
i.e. it is true iff at least one of the subconstraints is true.
- <code>$not</code> - given one constraint C, returns another constraint that is the "not" of C, i.e. it is true
iff C is false.
- <code>$true, $false</code> - takes no arguments, returns an "always true" or an "always false" constraint, respectively.
- <code>$if</code> - takes an "if", a "then", and optionally an "else", and returns an implies statement.
Given P and Q, returns P => Q, i.e. P or ~Q. Given P, Q, and R, returns (P => Q) ^ (~P => R).
- <code>$cond</code> - takes several if-then pairs (as one would use in <code>cond</code>), and composes together
several <code>$if</code> constraints. The final "else" clause can be specified with <code>:else</code> (like in <code>cond</code>),
or put as the last argument (like in <code>case</code> and <code>condp</code>).

- <code>$reify</code> - given a constraint C, will generate a boolean var V, such that V = 1 iff C.
- <code>$all-different?</code> - a constraint that specifies that several variables must end up with different values.
- <code>$circuit?</code> - a constraint that specifies that a given list L is a circuit, i.e. each item in the list
contains the index of the next item in the circuit. For example, <code>[1 2 3 4 0]</code> is a circuit because
<code>L[0]</code> contains 1, <code>L[1]</code> contains 2, <code>L[2]</code> contains 3, and if you follow the chain you'll
eventually visit every index once. You can also pass in an offset number to add to the indices (e.g. if you want to make the
array one-based).
- <code>$nth</code> - given a list L and an index i (a variable), will generate another variable that equals <code>L[i]</code>.
- <code>$satisfies-automaton?</code> - given an automaton (created with the following function) and a list of variables, returns
a constraint that specifies that the variables in sequence must satisfy the automaton.
- <code>automaton</code> - given a regular-expression-style string, returns a finite automaton to be used in <code>satisfies-automaton?</code>.
Example: <code>($automaton "(1|2)(3*)(4|5)")</code> means that a sequence of variables begins with 1 or 2, followed by
any number of 3's, ended with a 4 or 5. All digits (0-9) are treated as the numbers themselves, and any other character
(that isn't a paren, bar, star, plus, etc)
is treated as its unicode number. e.g. to write the number 10, you'd have to write <code>\u000A</code> (hex for 10).

#### Finding solutions

It's no fun to set up your variables and constraints and then not have a way to find the solution. There are a couple
ways to find solutions:

The first way is to call <code>solution</code>, which takes just a sequence of constraints.
It returns a solution map, whose keys are variable names, and whose values are the values of the variables.

	(solution [($int :x 1 5)
	           ($int :y 1 5)
	           ($= :x ($+ :y 4))]
	=> {:x 5, :y 1}

You can also call <code>solution</code> with keyword arguments to specify the optimization of a given variable
(or arithmetic expression).

	(solution [($int :x 1 5)
	           ($int :y 1 5)]
	          :maximize ($- :x :y))
	=> {:x 5, :y 1}

One idiosyncrasy I need to mention: if you use a <code>:minimize/:maximize</code> keyword, and the problem
is infeasible, the solving engine will find a solution anyway that bypasses some or all of the constraints.
This quirk is in the solver itself (which this library is a wrapper of), and therefore it was beyond my control.

Finally, you can get a lazy sequence of ALL of the solutions by calling <code>solutions</code>:

	(solutions [($int :x 1 5)
	            ($int :y 1 5)
	            ($= :x :y)]
	=> ({:x 1, :y 1}, {:x 2, :y 2}, {:x 3, :y 3}, {:x 4, :y 4}, {:x 5, :y 5})

## License

Copyright © 2013

Distributed under the Eclipse Public License, the same as Clojure.
