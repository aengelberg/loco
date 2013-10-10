(ns clj-cp.constraints
  (:require [clj-cp.core :as core])
  (:import (solver.constraints Arithmetic
                               ICF
                               )))

(defn arithm
  "Takes an arithmetic equation consisting of 2 or 3 variables with infix operators in between.
In the arglists, X and Y represent variables, const is a number, and ops can be strings or keywords.
In the case of 1 op, the op can be =, !=, <, >, <=, or >=.
In the case of 2 ops, the first op can be + or -, and the second op can be an equality/inequality specified above.
The first of two ops (if applicable) can be + or -."
  ([X op Y]
    (ICF/arithm X (name op) Y))
  ([X op1 Y op2 Z]
    (ICF/arithm X (name op1) Y (name op2) const))

(alter-meta! #'arithm update-in [:arglists] conj '[X op const])