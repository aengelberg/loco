(ns loco.core
  (:import (solver.variables VF)
           (solver.exception SolverException)
           solver.ResolutionPolicy))

(defn- namey?
  [x]
  (try (boolean (name x))
    (catch Exception e
      false)))

(defrecord LocoSolver
  [solver n-solutions my-vars]
  Object
  (toString [this] (str solver)))

(defn solver
  [name]
  (->LocoSolver
    (solver.Solver. name)
    (atom 0)
    (atom {})))

(def ^:dynamic current-solver-binding nil)
(defonce current-solver-atom (atom nil))

(defn- get-current-loco-solver []
  (or @current-solver-atom
      current-solver-binding
      (throw (Exception. "No \"solver\" binding found, try using with-solver"))))
(defn get-current-solver []
  (:solver (get-current-loco-solver)))
(defn- get-current-solution-n-atom []
  (:n-solutions (get-current-loco-solver)))
(defn- get-current-vars-atom []
  (:my-vars (get-current-loco-solver)))

(defmacro with-solver
  "A way of setting the current solver, using bindings.
Syntax:
(with-solver <my-solver>
  ...)
Note that, because bindings are in play behind the scenes, you can separate solver-dependent function calls into different functions as long as they are eventually nested in the initial bind.
See \"with-solver!\" for a more unsafe but convenient way to permanently set the solver binding without enclosing an expression.
At the moment, it is not a good idea to nest \"with-solver\" calls with the same solver."
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

(defn int-var
  "Creates an integer variable with the desired starting domain.
Variables with names starting with an underscore (\"_\") will be omitted from solution maps.
Omitting a name will result in an auto-generated name beginning with an underscore.
\"var-type\" is either :enumerated or :bounded (:enumerated by default).
An enumerated var explicitly stores all of the values in a Bit Set.
A bounded var only stores the min and max of the domain interval.
Sample usage:
(int-var \"x\" 1 5 :enumerated/:bounded)
(int-var \"x\" [1 2 3 4 5] :enumerated)"
  [& args]
  (let [[name? args] (if (namey? (first args))
                       [(first args) (rest args)]
                       [nil args])
        name (name (if name? name? (gensym "_intvar")))  ; note the funky usage of "name"
        [domain-expr args] (if (number? (first args))
                             [{:min (first args) :max (second args)} (rest (rest args))]
                             [(first args) (rest args)])
        domain-type (or (first args) :enumerated)
        v (case [domain-type (sequential? domain-expr)]
            [:enumerated false] (VF/enumerated name (:min domain-expr) (:max domain-expr)
                                               (get-current-solver))
            [:enumerated true] (VF/enumerated name (int-array domain-expr)
                                              (get-current-solver))
            [:bounded false] (VF/bounded name (:min domain-expr) (:max domain-expr)
                                         (get-current-solver))
            [:bounded true] (throw (Exception. "Bounded int-vars only take a min and a max")))]
    (swap! (get-current-vars-atom) update-in [:int] conj v)
    v))

(alter-meta! #'int-var assoc :arglists '([name? min max var-type?]
                                          [name? values var-type?]))

(defn const-var
  "Takes a number, and creates an object that behaves like an int-var but is in fact a constant number. The variable will not be included in a solution map.

Useful when using constraints that require a variable instead of a constant."
  ([n]
    (VF/fixed n (get-current-solver)))
  ([name n]
    (VF/fixed name n (get-current-solver))))

(defn bool-var
  "Returns a bool-var, which is essentially an int-var with a domain of [0;1]. Optionally takes a name."
  [& args]
  (let [[name? args] (if (namey? (first args))
                       [(first args) (rest args)]
                       [nil args])
        name (name (if name? name? (gensym "_boolvar")))  ; note the funky usage of "name"
        v (VF/bool name (get-current-solver))]
    (swap! (get-current-vars-atom) update-in [:bool] conj v)
    v))

(alter-meta! #'bool-var assoc :arglists '([name?]))

(defn get-val
  "Returns the one and only value in a variable's domain (otherwise the min, if there are multiple values)."
  [variable]
  (.getValue variable))

(defn- solution-map
  [solver n]
  (into {:solution n}
        (for [v (apply concat (vals @(get-current-vars-atom)))
              :let [n (.getName v)]
              :when (not= (first n) \_)]
          [n (get-val v)])))

(defn constrain!
  "Enforces a constraint to be true when it comes time to solve the variables.
Note that newly created constraints aren't actually being enforced until you call this function."
  ([constraint]
    (.post (get-current-solver)
      constraint))
  ([constraint & more]
    (doseq [c (cons constraint more)]
      (constrain! c))))

(defn- solve!*
  [solver & args]
  (let [args (apply hash-map args)]
    (cond
      (:maximize args) (do (.findOptimalSolution solver ResolutionPolicy/MAXIMIZE (:maximize args))
                         (swap! (get-current-solution-n-atom) inc)
                         true)
      (:minimize args) (do (.findOptimalSolution solver ResolutionPolicy/MINIMIZE (:minimize args))
                         (swap! (get-current-solution-n-atom) inc)
                         true)
      :else (try (and (.findSolution solver)
                      (swap! (get-current-solution-n-atom) inc)
                      true)
              (catch SolverException e
                (and (.nextSolution solver)
                     (swap! (get-current-solution-n-atom) inc)
                     true))))))

(defn solve!
  "Solves the solver using the posted constraints, and sets the variables' domains to the values (which you can retrieve with get-val).
Keyword arguments:
- :maximize <var> - finds the solution maximizing the given variable.
- :minimize <var> - finds the solution minimizing the given variable.
This function returns true if a solution is found or false if not.
When optimizing a variable, if the problem is infeasible, a solution will be found anyway that bypasses some or all of the constraints.
When not optimizing a variable, you can call this function multiple times, which will update the variables to the next solution, if any.

A useful idiom for imperatively iterating through all the solutions:
(while (solve!)
  <do stuff with variable assignments>)"
  [& args]
  (apply solve!* (get-current-solver) args))

(defn- solution*
  [solver & args]
  (let [solved? (apply solve!* solver args)]
    (when solved?
      (solution-map solver (dec @(get-current-solution-n-atom))))))

(defn solution
  "Solves the solver using the posted constraints and returns a map from variable names to their values (or nil if there is no solution).
You can call this function more than once, getting a new solution each time (like \"solve!\").
Keyword arguments:
- :maximize <var> - finds the solution maximizing the given variable.
- :minimize <var> - finds the solution minimizing the given variable.
When optimizing a variable, if the problem is infeasible, a solution will be found anyway that bypasses some or all of the constraints.
When not optimizing a variable, you can call this function multiple times, which will return new, updated solution maps representing the next solution (if any)."
  [& args]
  (apply solution* (get-current-solver) args))

(defn solutions
  "Solves the solver using the posted constraints and returns a lazy seq of maps (for each solution) from variable names to their values.
You shouldn't call this function more than once."
  []
  (let [solver (get-current-solver)]
    (take-while identity
                (repeatedly #(solution* solver)))))