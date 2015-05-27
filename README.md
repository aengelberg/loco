# loco

![CircleCI build status](https://circleci.com/gh/aengelberg/loco.png?circle-token=056c1eb8765d2e4e2cdfa0a0c143eee009ef172f)

Loco is a *Constraint Programming* library for Clojure.  Loco provides a fully *declarative, functional interface* to the Java library Choco.

    [loco "0.3.0"]

Loco requires JDK 8.

## What is Constraint Programming?

Constraint Programming is about solving problems that can be expressed in terms of *integer variables* and *constraints* on those variables.  For example, consider a problem in which variable `x` is an integer ranging from 1 to 6 and `y` is an integer ranging from 3 to 7.  We also know the constraint that `x+y=10`.  What are all the possible values for x and y?

Perhaps the simplest way to answer this question in Clojure is:

```clojure
(for [x (range 1 7),
      y (range 3 8)
      :when (= (+ x y) 10)]
  {:x x, :y y})
```

This approach tests every possible combination of x and y.  For this simple example, this approach suffices, but as the number of variables and constraints grows, testing every combination is typically not realistic.

A Constraint Programming (CP) engine uses a very specific strategy to search for the solutions in an extremely efficient manner.  First, the engine performs a step called "constraint propagation", in which it uses all the constraints to narrow down the possiblities for each variable.  For example, in this case, it is impossible for x to be 1.  Once everything has been narrowed down as much as possible, the engine picks a variable with the smallest number of possibilities and starts taking cases.  Within each case, it repeats the process of constraint propagation and then taking further subcases if necessary.  For many problems, this process eliminates an astonishing number of cases in a way that seems almost intelligent, reaching a solution in a very short amount of time.

Many CP engines are imperative in nature.  You create some mutable object that tracks the state of all the variables.  You add information about the variables and add constraints to the object.  Then, you kick off the solver.  At the end of the process, you inspect the variables to see what their final state is.

### Our first Loco program

Loco aims to provide a pleasing, concise, declarative way to express constraint problems.  `loco.core` exposes two main functions `solution` and `solutions`.  Each of these functions simply takes a sequence of *variable declarations* and *constraints*, in any order.

In CP lingo, a problem specification is often called a *model*.  Here is a model for our toy example:

```clojure
(use 'loco.core 'loco.constraints)

(def model
  [($in :x 1 6)  ; x is in the domain ranging from 1 to 6, inclusive
   ($in :y 3 7)  ; y is in the domain ranging from 3 to 7, inclusive
   ($= ($+ :x :y) 10)])
```

To find all the solutions:

```clojure
=> (solutions model)
({:y 7, :x 3} {:y 6, :x 4} {:y 5, :x 5} {:y 4, :x 6})
```

Notice that the model is nearly as compact as the corresponding `for` expression.  However, the model is fully declarative.  It is built with Loco functions, but ultimately, the model is simply a Clojure data structure describing all the variables and constraints.

```clojure
=> model
[{:domain {:min 1, :max 6},
  :type :int-domain,
  :can-init-var true,
  :name :x}
 {:domain {:min 3, :max 7},
  :type :int-domain,
  :can-init-var true,
  :name :y}
 {:eq "=",
  :arg2 10,
  :type :arithm-eq,
  :arg1 {:type :+, :args (:x :y), :id id1787, :can-optimize-eq #{}}}]
```

This is the Clojure philosophy of how to create a library: figure out how your problem can be expressed as data, write functions to build that kind of data, and then write functions to consume that kind of data.  This design principle has important consequences.

With some CP APIs, the problem needs to be written as one monolithic query.  However, with Loco, a model is simply a sequence of declarations and constraints so, for example, we can write separate functions to produce different aspects of a model, and then concatenate them.  With some CP APIs, variables are mutated by the solving process.  But with Loco, the model is immutable -- it is easy to make speculative variations of the model and run them through the solver.

All of the Loco functions to build these data structures begin with a `$`, in order to make it easier to `use` rather than `require` loco.constraints without name collision.  This is handy because so many of the functions share names with arithmetic and other core functions.

## Concepts

### Names

A variable must have a name.  There are two kinds of names:

1. Keywords.  For example, `:x` and `:y` are valid variable names.
2. Vectors beginning with a keyword.  For example, `[:x 1]` and `[:y "max"]` are valid variable names.  Conceptually, it helps to think of these as subscripted variables, such as
<a href="http://www.codecogs.com/eqnedit.php?latex=%5Cinline&space;$x_1$" target="_blank"><img src="http://latex.codecogs.com/gif.latex?%5Cinline&space;$x_1$" title="$x_1$" /></a> and <a href="http://www.codecogs.com/eqnedit.php?latex=%5Cinline&space;$y_%5Ctextrm{max}$" target="_blank"><img src="http://latex.codecogs.com/gif.latex?%5Cinline&space;$y_%5Ctextrm{max}$" title="$y_%5Ctextrm{max}$" /></a>.

If a name starts with an underscore character, it will be omitted from the final solution map.

```clojure
=> (solution [($in :x 0 1)
              ($in :_y 0 1)
              ($= ($+ :x :_y) 2)])
{:x 1}
```
    
### Variables

Every variable must be declared at least once in the model, for example,

```clojure
($in :x 1 10)
```

states that `:x` is an integer ranging from 1 to 10. You can also explicitly state the values of a domain, like so:

```clojure
($in :x [1 2 3 4 5 6 7 8 9 10])
```

Creating vars with the former method will be equivalent to the latter method in the long run (as the values are stored explicitly either way). The latter method is less concise but more flexible.

It is common in Constraint Programming to express boolean variables as numbers that can be either 0 or 1.  So to create a boolean variable b:

```clojure
($in :b 0 1)
```

Typically, variables are represented internally by the solving engine as a set of all the remaining  values which are valid.  However, the engine supports another representation which tracks only the minimal and maximal value that the variable can take.  To enable this option, use the `:bounded` keyword:

```clojure
($in :x 1 10 :bounded)
```

Generally speaking, this option is more space efficient at the expense of time.

`$in` can also be used as a constraint within nested expressions, for example,

```clojure
($or ($in :x 1 5) ($in :x 6 10))
```

is a valid constraint.  However, it is still essential that somewhere in the model, the variable must be declared unnested, at the "top level".

So this by itself is not a valid model, because `:x` is not declared:

```clojure
[($or ($in :x 1 5) ($in :x 6 10))]
```

but this is:

```clojure
[($in :x 1 10)
 ($or ($in :x 1 5) ($in :x 6 10))]
```

### Expression Nesting

Imagine you'd like to specify `A + (B * C) = D` in your CP model. In Choco (the Java library),
the code looks something like this:

```java
IntVar x = VariableFactory.integer("x", 1, 10, solver);
solver.post(ICF.times(b, c, x));
solver.post(ICF.arithm(a, "+", x, "=", d));
```

This is surprisingly inelegant for a relatively simple expression.
Even when looking past the inherent Java messiness, there's a deeper problem, which
is that I had to create an intermediate variable, `x`, to attach to the expression `B * C`.
This way of programming forces me to think from the inside out.

In Loco, that expression can be written in one line as it should:

```clojure
($= ($+ :a ($* :b :c)) :d)
```

This is far closer to the mathematical expression we started with. When this
constraint is consumed by a Loco function such as `solution`, intermediate variables are
automatically created behind the scenes. Those variables have auto-generated names beginning with
`_`, so you can't see them in the solution map.

For instance, given the following model:

```clojure
[($in :a 1 10)
 ($in :b 4 8)
 ($in :c 1 5)
 ($in :d 3 10)
 ($= ($+ :a ($* :b :c)) :d)]
```

Behind the scenes, Loco is feeding Choco the following pseudo-program:

    a ɛ [1,10]
    b ɛ [4,8]
    c ɛ [1,5]
    d ɛ [3,10]
    _int-var8624 ɛ [4,40]
    b + c = _int-var8624
    a + _int-var8624 = d

There are a couple things to note here:

* Loco has to automatically guess a new starting domain for `_int-var8624` based on the domains of `b` and `c`.
It can do this by guessing the new minimum and maximum, depending on `b` and `c`'s domain mins / maxes and the operation
you are calling on the variables. In this case, the absolute possible minimum of `b * c` must be 4 * 1 = 4, and the
maximum is 8 * 5 = 40.
* A temporary variable did not have to be created for the addition, because Loco recognized the opportunity to use the direct `x + y = z`
constraint already provided by Choco. I've implemented some pre-defined shortcuts, so that in general, `($= ($some-operator ...) x)`
will result in one constraint, not multiple.

### Constraints

Here is a complete list of all of the constraints available to you.
Functions marked with an asterisk (*) are nestable expressions (as described in the section above).

#### Numeric Constraints

- `$+`* - given a mixture of variables / numbers, returns the sum.
- `$-`* - given a mixture of variables / numbers, returns `X - Y - Z - ...`,
or `-X` if only one argument is given.
- `$*`* - given two arguments, returns the product. One argument can be a constant number >= -1.
- `$min`* - returns the minimum of several arguments.
- `$max`* - returns the maximum of several arguments.
- `$mod`* - given two arguments X and Y, returns X mod Y.
- `$scalar`* - given a list of variables (X, Y, Z, ...) and a list of integer coefficients (a, b, c, ...)
returns `aX + bY + cZ + ...`.
- `$abs`* - given a variable X, returns the absolute value of X.

- `$=, $<, $>, $<=, $>=, $!=` - constraints that specify equality/inequality between two or more arguments.
Calling these on more than two arguments will return a composition of multiple constraints (which collectively have
same functionality, but might be less efficient than you'd like).

#### Logical Constraints

- `$and` - given zero or more constraints, returns another constraint that is the "and" of the subconstraints,
i.e. it is true iff all of the subconstraints is true.
- `$or` - given zero or more constraints, returns another constraint that is the "or" of the subconstraints,
i.e. it is true iff at least one of the subconstraints is true.
- `$not` - given one constraint C, returns another constraint that is the "not" of C, i.e. it is true
iff C is false.
- `$true, $false` - takes no arguments, returns an "always true" or an "always false" constraint, respectively.
- `$if` - takes an "if", a "then", and optionally an "else", and returns an implies statement.
Given P and Q, returns P => Q, i.e. if P is true, Q is true (but not necessarily the other direction).
Given P, Q, and R, returns (P => Q) ^ (~P => R).
- `$cond` - takes several if-then pairs (as one would use in `cond`), and composes together
several `$if` constraints. The final "else" clause can be specified with `:else` (like in `cond`),
or put as the last argument (like in `case` and `condp`).
- `$reify`* - given a constraint C, will generate a boolean var V, such that V = 1 iff C.

#### Global Constraints

These constraints look for meta-relationships between multiple variables.

- `$distinct` - a constraint that specifies that several variables must end up with different values.
- `$circuit` - a constraint that specifies that a given list L is a circuit, i.e. each item in the list
contains the index of the next item in the circuit. For example, `[1 2 3 4 0]` is a circuit because
`L[0]` contains 1, `L[1]` contains 2, `L[2]` contains 3, and if you follow the chain you'll
eventually visit every index once. You can also pass in an offset number to add to the indices (e.g. if you want to make the
array one-based).
- `$nth`* - given a list L and an index i (a variable), will generate another variable that equals `L[i]`.
- `$cardinality` - takes a list of variables, and a frequency map with numbers as keys, and variables/numbers as values. Returns a constraint
that ensures that for each k,v pair in the map, k appears v times in the list of variables.
Example: `($cardinality [:a :b :c :d] {1 :how-many-ones, 2 :how-many-twos})` could yield a solution `{:a 1, :b 1, :c 2, :d 3, :how-many-ones 2, :how-many-twos 1}`.
Also takes an optional keyword argument, `:closed true` (default false), which ensures that the list ONLY contains keys that are in the frequency map.
- `$knapsack` - takes a list of weights `w_1, w_2, ...`, a list of values `v_1, v_2, ...`, a list of variables `O_1, O_2, ...`, and variables `W_total` and `V_total`. Constrains that `sum(O_i * w_i) = W_total`, and that `sum(O_i * v_i) = V_total`.
- `$regex` - given a rudimentary regular expression and a list of variables, constrains that said variables in sequence
must follow the regex. Values (non-terminals) in the regex are represented as characters, though are reflected in the int-vars
as the ASCII values. The exception is that digit characters are reflected as the digits themselves.
Example: `($regex "a5" [:x :y]) => {:x 97, :y 5}`. Here is a more technical list of the syntax of the regular expression.
 - any character - the ASCII value of the character
 - `0` to `9` - the number itself
 - `(...)` - grouping
 - `a|b` - alternation / or
 - `a*` - zero or more
 - `a+` - one or more
 - `a?` - zero or one
 - `[abc]` - one of several characters
 - `[a-z]` - character range
 - `\( \[ \* etc.` - escape special character

Whitespace characters are also treated as unicode numbers; they are not ignored by the parser.

## Finding solutions

There are a couple ways to find the solution(s) to a Loco model you have constructed.

The first way is to call `solution`. It returns a solution map, whose keys are variable names, and whose values are the values of the variables.

```clojure
=> (solution [($in :x 1 5)
              ($in :y 1 5)
              ($= :x ($+ :y 4))])
{:x 5, :y 1}
```

You can also call `solution` with keyword arguments to specify the optimization of a given variable
(or arithmetic expression).

```clojure
=> (solution [($in :x 1 5)
              ($in :y 1 5)]
             :maximize ($- :x :y))
{:x 5, :y 1}
```

You can get a lazy sequence of ALL of the solutions by calling `solutions`:

```clojure
=> (solutions [($in :x 1 5)
               ($in :y 1 5)
               ($= :x :y)])
({:x 1, :y 1}, {:x 2, :y 2}, {:x 3, :y 3}, {:x 4, :y 4}, {:x 5, :y 5})
```

You can also use the `:minimize/:maximize` keyword on `solutions`.
This will find all of the optimal solutions, in a non-lazy sequence:

```clojure
=> (solutions [($in :x -5 5)
               ($in :y -5 5)]
              :minimize ($* :x :y))
({:x 5, :y -5} {:x -5, :y 5})
```

To time-out the solution early, use the `:timeout` argument. After the specified amount of milliseconds,
- if calling `solution`, returns `nil`;
- if calling `solution` with a `:minimize` or `:maximize` keyword, returns the best solution found so far;
- if calling `solutions`, ends the lazy sequence early.
- if calling `solutions` with a `:minimize` or `:maximize` keyword, ends the list of optimal solutions early.

Note that this timeout is not a guarantee -- the timer is only checked between propagations, which usually but not always
results in a punctual termination.

```clojure
=> (solution [($in :x 1 10)
              ($in :y 1 10)
              ($= :x :y)]
             :maximize :x
             :timeout 1000)   ; timeout after 1 second. (pretend this is a super hard problem that takes a few seconds)
{:x 7, :y 7}
```

## About Loco

Loco was born out of a desire to create a constraint solver that would improve upon core.logic by leveraging an existing Java constraint library to provide better performance and a wider range of constraints.  The first iteration, [CloCoP](http://github.com/aengelberg/clocop), was based upon the JaCoP library, and introduced the "piping" operators, i.e., nested constraints which automatically produce other internal variables.

Shortly after announcing CloCoP, I discovered the Choco library. Choco has a couple of features that makes it slightly better than JaCoP for this situation, such as lazy evaluation of multiple solutions, so I decided to create a new version of CloCoP based on Choco.  Along the way, I took this as an opportunity to experiment with the modeling API, and eventually moved to a purely declarative approach.  The new API was sufficiently different from CloCoP that it warranted a new name: Loco.

## Credits

Alex Engelberg is the author and maintainer of the Loco library.  Issues, examples, and pull requests are welcome.

Thanks to [Andrea Richiardi](https://github.com/arichiardi) for contributing Loco solutions for sample Constraint Programming problems. His examples can be can be found in the test directory.

Also, thanks to the Seattle Clojure group (Seajure), who listened as I explained an early iteration of Loco and nudged me in the direction of being fully declarative.

Special thanks to Charles Prud'homme, a maintainer of Choco, for permission to create a Clojure spin-off, as well as additional guidance with some Choco features.

## License

Copyright © 2014

Distributed under the Eclipse Public License, the same as Clojure.
