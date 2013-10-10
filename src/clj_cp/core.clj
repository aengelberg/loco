(ns clj-cp.core
  (:import (solver.variables VF)
           (solver.exception SolverException)))

(defn solver
  [name]
  (solver.Solver. name))

(def ^:dynamic current-solver nil)

(defmacro with-solver
  [solver & exprs]
  `(binding [current-solver ~solver]
     ~@exprs))

(defn get-current-solver []
  (if current-solver
    current-solver
    (throw (Exception. "No \"solver\" binding found, try using with-solver"))))

(defn int-var
  "Creates an integer variable with the desired starting domain.
Omit the \"name\" field for an auto-generated name.
\"var-type\" is either :enumerated or :bounded (:enumerated by default).
An enumerated var explicitly stores all of the values in a Bit Set.
A bounded var only stores the min and max of the domain interval."
  [& args]
  (let [[name? args] (if (string? (first args))
                       [(first args) (rest args)]
                       [nil args])
        name (if name? name? (gensym "intvar"))
        [domain-expr args] (if (number? (first args))
                             [{:min (first args) :max (second args)} (rest (rest args))]
                             [(first args) (rest args)])
        domain-type (or (first args) :enumerated)]
    (case [domain-type (sequential? domain-expr)]
      [:enumerated false] (VF/enumerated name (:min domain-expr) (:max domain-expr)
                                         (get-current-solver))
      [:enumerated true] (VF/enumerated name (int-array domain-expr)
                                        (get-current-solver))
      [:bounded false] (VF/bounded name (:min domain-expr) (:max domain-expr)
                                   (get-current-solver))
      [:bounded true] (throw (Exception. "Bounded int-vars only take a min and a max")))))

(defn const-var
  "Takes a number, and creates an object that behaves like an int-var but is in fact a constant number.

Useful when using constraints that require a variable instead of a constant.

NOTE: the \"variable\" returned from this function will not be shown in a solution map."
  ([n]
    (VF/fixed n (get-current-solver)))
  ([name n]
    (VF/fixed name n (get-current-solver))))

(alter-meta! #'int-var assoc :arglists '([name? min max var-type?]
                                          [name? values var-type?]))

(defn bool-var
  "An int-var with a domain of [0;1]."
  [& args]
  (let [[name? args] (if (string? (first args))
                       [(first args) (rest args)]
                       [nil args])
        name (if name? name? (gensym "intvar"))]
    (VF/bool name (get-current-solver))))

(alter-meta! #'bool-var assoc :arglists '([name?]))

(defn get-val
  [variable]
  (.getValue variable))

(defn- solution-map
  [solver n]
  (into {:solution n}
        (for [v (.getVars solver)]
          [(.getName v) (.getValue v)])))

(defn- lazy-solutions
  ([solver]
    (if (.findSolution solver)
      (cons (solution-map solver 0)
            (lazy-seq (lazy-solutions solver 1)))
      ()))
  ([solver n]
    (if (.nextSolution solver)
      (cons (solution-map solver n)
            (lazy-seq (lazy-solutions solver (inc n))))
      ())))

(defn constrain!
  [constraint]
  (.post (get-current-solver)
    constraint))

(defn solve!
  "Solves for the variables. Use (get-val <variable>) to get the values of the variables you want.
You can call this function again, which will update the variables to the next available solution.

Returns true if the next solution was found, returns false if there are no more solutions.

A useful idiom for imperatively iterating through all the solutions:
(while (solve!)
  <do stuff with variables>)"
  []
  (let [solver (get-current-solver)]
    (try (.findSolution solver)
      (catch SolverException e
        (.nextSolution solver)))))

(defn solution
  "Solves the solver using the posted constraints and returns a map from variable names to their values. You shouldn't call this function more than once."
  []
  (let [solver (get-current-solver)]
    (do (.findSolution solver)
      (solution-map 0))))

(defn solutions
  "Solves the solver using the posted constraints and returns a lazy seq of maps (for each solution) from variable names to their values. You shouldn't call this function more than once."
  []
  (let [solver (get-current-solver)]
    (lazy-solutions solver)))