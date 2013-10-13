(ns clj-cp.constraints
  (:require [clj-cp.core :as core])
  (:import (solver.constraints Arithmetic
                               ICF
                               LCF
                               Constraint)
           (solver.variables IntVar
                             BoolVar
                             VF)))

(defn arithm
  "Takes an arithmetic equation consisting of 2 or 3 variables with infix operators in between.
In the arglists, X and Y represent variables, const is a number, and ops can be strings or keywords.
In the case of 1 op, the op can be =, !=, <, >, <=, or >=.
In the case of 2 ops, the first op can be + or -, and the second op can be an equality/inequality specified above.
The first of two ops (if applicable) can be + or -."
  ([X op Y]
    (ICF/arithm X (name op) Y))
  ([X op1 Y op2 const]
    (ICF/arithm X (name op1) Y (name op2) const)))

(alter-meta! #'arithm update-in [:arglists] conj '[X op const])

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
          lo (if (number? v) v (.getLB (first vars)))
          hi (if (number? v) v (.getUB (first vars)))]
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

(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if and only if every subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    (.TRUE (core/get-current-solver))
    (LCF/and (into-array Constraint constraints))))

(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and only if at least one subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    (.FALSE (core/get-current-solver))
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

(defn $reify
  "Given a constraint C, returns a bool-var V such that (V = 1) iff C."
  [C]
  (let [V (core/bool-var (gensym "reify"))]
    (core/constrain! (LCF/reification V C))
    V))

(defn $all-different
  "Given an arbitrary amount of int-vars, ensures that all variables have different values, i.e. no two of them are equal."
  [& vars]
  (ICF/alldifferent (into-array IntVar vars) "DEFAULT"))