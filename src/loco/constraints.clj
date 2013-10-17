(ns loco.constraints
  (:require [loco.core :as core])
  (:import (solver.constraints Arithmetic
                               ICF
                               LCF
                               Constraint)
           (solver.variables IntVar
                             BoolVar
                             VF)
           solver.constraints.nary.automata.FA.FiniteAutomaton))

(defn- domain-min
  [x]
  (if (number? x)
    x
    (.getLB x)))

(defn- domain-max
  [x]
  (if (number? x)
    x
    (.getUB x)))

;;;;; ARITHMETIC

(defn $arithm
  "Takes an arithmetic equation consisting of 2 or 3 variables with infix operators in between.
In the arglists, X and Y represent variables, const is a number, and ops can be strings or keywords.
In the case of 1 op, the op can be =, !=, <, >, <=, or >=.
In the case of 2 ops, the first op can be + or -, and the second op can be an equality/inequality specified above.
The first of two ops (if applicable) can be + or -."
  ([X op Y]
    (ICF/arithm X (name op) Y))
  ([X op1 Y op2 const]
    (ICF/arithm X (name op1) Y (name op2) const)))

(alter-meta! #'$arithm update-in [:arglists] conj '[X op const])

(defn- $+view
  [x const]
  (VF/offset x const))

(defn $+
  "Takes a number of int-vars and returns another int-var which is constrained to equal the sum.
Or pass in a variable \"X\" and a number \"const\", and get an int-view of X + const."
  ([x]
    x)
  ([x y & more]
    (cond
      (and (empty? more)
           (number? y)) ($+view x y)
      (and (empty? more)
           (number? x)) ($+view y x)
      :else (let [vars (list* x y more)
                  mins (map #(.getLB ^IntVar %) vars)
                  maxes (map #(.getUB ^IntVar %) vars)
                  sum-var (core/int-var (apply + mins) (apply + maxes))]
              (core/constrain! (ICF/sum (into-array IntVar vars) sum-var))
              sum-var))))

(alter-meta! #'$+ assoc :arglists '([x const] [const x] [x y & more]))

(defn $-
  "Takes a number of int-vars, and returns another int-var constrained to equal (x - y - z - ...) or (-x) if only one argument is given.
When negating a single variable, this function returns an int-view.
As in $+, you can pass in one var and one number and get an int-view."
  ([x]
    (if (number? x)
      (- x)
      (VF/minus x)))
  ([x & more]
    (apply $+ x (map $- more))))

(defn- $*view
  [x const]
  (VF/scale x const))

(defn- keypoints
  [vars op neutral]
  (if (empty? vars)
    neutral
    (let [v (first vars)
          lo (domain-min v)
          hi (domain-max v)]
      (concat (op lo (keypoints (rest vars) op neutral))
              (op hi (keypoints (rest vars) op neutral))))))

(defn $*
  "Pass in two int-vars and get a new int-var constrained to equal the product of the two vars.
Or, pass in a var and a number (the number is greater than or equal to -1) and get an int-view of the product of the two items."
  ([x y]
    (cond
      (number? y) ($*view x y)
      (number? x) ($*view y x)
      :else (let [nums (keypoints [x y] * 1)
                  total-min (apply min nums)
                  total-max (apply max nums)
                  z (core/int-var total-min total-max)]
              (core/constrain! (ICF/times x y z))))))

(alter-meta! #'$* assoc :arglists '([x const] [const x] [x y]))

(defn $min
  "Returns a new int-var constrained to equal the minimum of the supplied int-vars."
  [& args]
  (let [mins (map domain-min args)
        maxes (map domain-max args)
        final-min (apply min mins)
        final-max (apply min maxes)
        new-var (core/int-var final-min final-max)]
    (core/constrain!
      (if (= (count args) 2)
        (ICF/minimum new-var (first args) (second args))
        (ICF/minimum new-var (into-array IntVar args))))
    new-var))

(defn $max
  "Returns a new int-var constrained to equal the maximum of the supplied int-vars."
  [& args]
  (let [mins (map domain-min args)
        maxes (map domain-max args)
        final-min (apply max mins)
        final-max (apply max maxes)
        new-var (core/int-var final-min final-max)]
    (core/constrain!
      (if (= (count args) 2)
        (ICF/maximum new-var (first args) (second args))
        (ICF/maximum new-var (into-array IntVar args))))))

(defn $mod
  "Given int-vars X and Y, returns a new int-var Z constrained to equal X mod Y."
  [X Y]
  (let [Ymax (domain-max Y)
        Z (core/int-var 0 (max (dec Ymax) 0))]
    (core/constrain! (ICF/mod X Y Z))
    Z))

(defn $scalar
  "Given a list of variables X, Y, Z, etc. and a list of number coefficients a, b, c, etc. returns a new variable constrained to equal aX + bY + cZ ..."
  [variables coefficients]
  (let [minmaxes (for [[v c] (map vector variables coefficients)
                       :let [dmin (* (domain-min v) c)
                             dmax (* (domain-max v) c)]]
                   (if (< dmin dmax)
                     [dmin dmax]
                     [dmax dmin]))
        final-min (apply min (map first minmaxes))
        final-max (apply max (map second minmaxes))
        new-var (core/int-var final-min final-max)]
    (core/constrain! (ICF/scalar (into-array IntVar variables) (int-array coefficients) new-var))))

(declare $not $and)

(defmacro ^:private defn-equality-constraint
  [name docstring str]
  `(defn ~name
     ~docstring
     ([X# Y#]
       ($arithm X# ~str Y#))
     ([X# Y# & more#]
       (apply $and (map (partial apply ~name) (partition 2 1 (list* X# Y# more#)))))))

(defn-equality-constraint $=
  "Constrains that X = Y.
Giving more than 2 inputs results in an $and statement with multiple $= statements."
  "=")
(defn-equality-constraint $<
  "Constrains that X < Y.
Giving more than 2 inputs results in an $and statement with multiple $< statements."
  "<")
(defn-equality-constraint $>
  "Constrains that X > Y.
Giving more than 2 inputs results in an $and statement with multiple $> statements."
  ">")
(defn-equality-constraint $<=
  "Constrains that X <= Y.
Giving more than 2 inputs results in an $and statement with multiple $<= statements."
  "<=")
(defn-equality-constraint $>=
  "Constrains that X >= Y.
Giving more than 2 inputs results in an $and statement with multiple $>= statements."
  ">=")
(defn $!=
  "Constrains that X != Y.
Giving more than 2 inputs (X, Y, Z, ...) results in NOT([X = Y] ^ [Y = Z] ^ ...)"
  ([X Y]
    ($arithm X "!=" Y))
  ([X Y Z & more]
    ($not (apply $= X Y Z more))))

;;;;; LOGIC

(defn $true
  "The resulting constraint is always true. Sometimes useful as a dummy constraint inside logic constraints."
  []
  (ICF/TRUE (core/get-current-solver)))

(defn $false
  "The resulting constraint is always false. Sometimes useful as a dummy constraint inside logic constraints."
  []
  (ICF/FALSE (core/get-current-solver)))

(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if and only if every subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($true)
    (LCF/and (into-array Constraint constraints))))

(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and only if at least one subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($false)
    (LCF/or (into-array Constraint constraints))))

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\"; not-C is true iff C is false."
  [C]
  (LCF/not C))

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole statement is false).
An optional \"else\" field can be specified, which must be true if P is false."
  ([if-this then-this]
    (LCF/ifThen if-this then-this))
  ([if-this then-this else-this]
    (LCF/ifThenElse if-this then-this else-this)))

(defn $cond
  "A convenience function for constructing a \"cond\"-like statement out of $if statements.
The final \"else\" can be specified by itself (being the odd argument) or with the :else keyword.

Example:
($cond P Q, R S, :else T)
=> ($if P Q ($if R S T))

If no \"else\" clause is specified, it is \"True\" by default."
  [& clauses]
  (cond
    (empty? clauses) ($true)
    (empty? (rest clauses)) (first clauses)
    (empty? (rest (rest clauses))) (if (= (first clauses) :else)
                                     (second clauses)
                                     (apply $if clauses))
    :else ($if (first clauses) (second clauses) (apply $cond (rest (rest clauses))))))

(defn $reify
  "Given a constraint C, returns a bool-var V such that (V = 1) iff C."
  [C]
  (let [V (core/bool-var (gensym "_reify"))]
    (core/constrain! (LCF/reification V C))
    V))

;;;;; GLOBAL

(defn $all-different?
  "Given a bunch of int-vars, ensures that all of them have different values, i.e. no two of them are equal."
  [& vars]
  (ICF/alldifferent (into-array IntVar vars) "DEFAULT"))

(defn $circuit?
  "Given a list of int-vars L, and an optional offset number (default 0), the elements of L define a circuit, where
(L[i] = j + offset) means that j is the successor of i.
Hint: make the offset 1 when using a 1-based list."
  ([list-of-vars]
    ($circuit? list-of-vars 0))
  ([list-of-vars offset]
    (ICF/circuit (into-array IntVar list-of-vars) offset)))

(defn $nth
  "Given a list of int-vars L, an int-var i, and an optional offset number (default 0), returns a new int-var constrained to equal L[i], or L[i - offset]."
  ([list-of-vars index-var]
    ($nth list-of-vars index-var 0))
  ([list-of-vars index-var offset]
    (let [final-min (apply min (map domain-min list-of-vars))
          final-max (apply max (map domain-max list-of-vars))
          new-var (core/int-var final-min final-max)]
      (core/constrain! (ICF/element new-var (into-array IntVar list-of-vars) index-var offset))
      new-var)))

(defn $automaton
  "Returns a Finite Automaton based on a supplied regular expression, with characters as the non-terminals.
Example of regexp: (1|2)(3*)(4|5)
Numbers (from 0 to 9) are treated as the numbers themselves; non-digit characters are converted to their unicode numbers.
If you wanted to use the number 10 (which would normally be read as 1, 0), you'd have to type \\u000A (hexadecimal for 10).
Be careful to not use spaces; they'll be treated as their ASCII value 32!"
  [regexp]
  (FiniteAutomaton. regexp))

(defn $satisfies-automaton?
  "Given a list of variables and an automaton (created with $automaton), constrains that the list of variables in succession results in a final state in the automaton."
  [list-of-vars automaton]
  (ICF/regular (into-array IntVar list-of-vars) automaton))