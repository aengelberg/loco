(ns clj-cp.core
  (:import (solver.variables VF)
           (solver.exception SolverException)))

(defn- namey?
  [x]
  (try (boolean (name x))
    (catch Exception e
      false)))

(defn solver
  [name]
  (solver.Solver. name))

(def ^:private ^:dynamic current-solver-binding nil)
(def ^:private current-solver-atom (atom nil))

(defmacro with-solver
  "A way of setting the current solver, using bindings.
Syntax:
(with-solver <my-solver>
  ...)
Note that, because bindings are in play behind the scenes, you can separate solver-dependent function calls into different functions as long as they are eventually nested in the initial bind.
See \"with-solver!\" for a more unsafe but convenient way to permanently set the solver binding without enclosing an expression."
  [solver & exprs]
  `(binding [current-solver-binding ~solver]
     ~@exprs))

(defn with-solver!
  "A mutable way of setting the current solver, using atoms. This is somewhat unwise when solving multiple CP problems, especially when nesting them.
You can use this for convenience, if you're testing things out at the REPL and you'd like to type each constraint / var declaration line-by-line.
Calling (with-solver! nil) effectively undoes the operation.
A \"with-solver!\" call overrides a \"with-solver\" call if both are active.
Syntax:
(with-solver! <my-solver>)
..."
  [solver]
  (reset! current-solver-atom solver))

(defn get-current-solver []
  (or @current-solver-atom
      current-solver-binding
      (throw (Exception. "No \"solver\" binding found, try using with-solver"))))

(defn int-var
  "Creates an integer variable with the desired starting domain.
Variables with names starting with an underscore (\"_\") will be ommitted from solution maps.
Omitting the \"name\" field will result in an auto-generated name beginning with an underscore.
\"var-type\" is either :enumerated or :bounded (:enumerated by default).
An enumerated var explicitly stores all of the values in a Bit Set.
A bounded var only stores the min and max of the domain interval."
  [& args]
  (let [[name? args] (if (namey? (first args))
                       [(first args) (rest args)]
                       [nil args])
        name (name (if name? name? (gensym "_intvar")))  ; beware the funky usage of "name"
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
  "Takes a number, and creates an object that behaves like an int-var but is in fact a constant number. The variable will not be included in a solution map.

Useful when using constraints that require a variable instead of a constant."
  ([n]
    (VF/fixed n (get-current-solver)))
  ([name n]
    (VF/fixed name n (get-current-solver))))

(alter-meta! #'int-var assoc :arglists '([name? min max var-type?]
                                          [name? values var-type?]))

(defn bool-var
  "An int-var with a domain of [0;1]."
  [& args]
  (let [[name? args] (if (namey? (first args))
                       [(first args) (rest args)]
                       [nil args])
        name (name (if name? name? (gensym "boolvar")))]  ; beware the funky usage of "name"
    (VF/bool name (get-current-solver))))

(alter-meta! #'bool-var assoc :arglists '([name?]))

(defn get-val
  [variable]
  (.getValue variable))

(defn- solution-map
  [solver n]
  (into {:solution n}
        (for [v (.getVars solver)
              :let [n (.getName v)]
              :when (not= (first n) \_)]
          [n (.getValue v)])))

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
  "Enforces a constraint to be true when it comes time to solve the variables.
Note that newly created constraints aren't actually being enforced until you call this function."
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