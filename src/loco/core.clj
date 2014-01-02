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
  [csolver n-solutions my-vars]
  Object
  (toString [this] (str csolver)))

(defmethod print-method
  LocoSolver
  [this ^java.io.Writer w]
  (.write w (str "#" `LocoSolver "{" (:csolver this) "}")))

(defn solver
  ([name]
    (->LocoSolver
      (solver.Solver. name)
      (atom 0)
      (atom {})))
  ([]
    (solver (str (gensym "solver")))))

(defn find-int-var
  [solver x]
  (@(:my-vars solver) (name x)))

(defmulti eval-constraint-expr
  "Evaluates a map data structure in the correct behavior, typically returning a constraint or a variable."
  (fn [data solver]
    (or (:type data) (type data))))

(defmethod eval-constraint-expr :default
  [data solver]
  data)

(defmethod eval-constraint-expr String
  [data solver]
  (find-int-var solver data))

(defmethod eval-constraint-expr clojure.lang.Keyword
  [data solver]
  (find-int-var solver data))

(defmethod eval-constraint-expr clojure.lang.Symbol
  [data solver]
  (find-int-var solver data))

(defn
  ^{:arglists '([solver name? min max var-type?]
                 [solver name? values])}
  int-var
  "Creates an integer variable with the desired starting domain.
\"var-type\" is either :enumerated or :bounded (:enumerated by default).
An enumerated var explicitly stores all of the values in a Bit Set.
A bounded var only stores the min and max of the domain interval.
Sample usage:
(int-var \"x\" 1 5)
(int-var \"x\" 1 5 :bounded)
(int-var \"x\" [1 2 3 4 5])"
  [solver & args]
  (let [[var-name? args] (if (namey? (first args))
                           [(first args) (rest args)]
                           [nil args])
        var-name (name (if var-name? var-name? (gensym "_intvar")))
        [domain-expr args] (if (number? (first args))
                             [{:min (first args) :max (second args)} (rest (rest args))]
                             [(first args) (rest args)])
        domain-type (or (first args) :enumerated)
        v (case [domain-type (sequential? domain-expr)]
            [:enumerated false] (VF/enumerated var-name (:min domain-expr) (:max domain-expr)
                                               (:csolver solver))
            [:enumerated true] (VF/enumerated var-name (int-array (sort domain-expr))
                                              (:csolver solver))
            [:bounded false] (VF/bounded var-name (:min domain-expr) (:max domain-expr)
                                         (:csolver solver))
            [:bounded true] (throw (Exception. "Bounded int-vars only take a min and a max")))]
    (swap! (:my-vars solver) assoc var-name v)
    v))

(defn const-var
  "Takes a number, and creates an object that behaves like an int-var but in fact contains a constant number.

Useful when using constraints that require a variable instead of a constant."
  ([solver n]
    (const-var solver (gensym "_const") n))
  ([solver var-name n]
    (let [v (VF/fixed (name var-name) n (:csolver solver))]
      (swap! (:my-vars solver) assoc var-name v)
      v)))

(defn
  ^{:arglists '([solver name?])}
  bool-var
  "Returns a bool-var, which is essentially an int-var with a domain of [0;1]. Optionally takes a name."
  [solver & args]
  (let [[var-name? args] (if (namey? (first args))
                           [(first args) (rest args)]
                           [nil args])
        var-name (name (if var-name? var-name? (gensym "_boolvar")))
        v (VF/bool var-name (:csolver solver))]
    (swap! (:my-vars solver) assoc var-name v)
    v))

(defn get-val
  "Returns the one and only value in a variable's domain (otherwise the min, if there are multiple values)."
  ([variable]
    (.getValue variable))
  ([store var-name]
    (.getValue (@(:my-vars store) var-name))))

(defn- solution-map
  [solver n]
  (into (with-meta {} {:loco/solution n})
        (for [v (vals @(:my-vars solver))
              :let [n (.getName v)]
              :when (not= (first n) \_)]
          [n (get-val v)])))

(defn constrain!
  "Enforces a constraint to be true when it comes time to solve the variables.
Note that newly created constraints aren't actually being enforced until you call this function."
  ([solver constraint]
    (.post (:csolver solver)
      (eval-constraint-expr constraint solver)))
  ([solver constraint & more]
    (doseq [c (cons constraint more)]
      (constrain! solver c))))

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
  [solver & args]
  (let [args (apply hash-map args)
        n-atom (:n-solutions solver)
        solver (:csolver solver)]
    (cond
      (:maximize args) (do (.findOptimalSolution solver ResolutionPolicy/MAXIMIZE (eval-constraint-expr (:maximize args)))
                         (swap! n-atom inc)
                         true)
      (:minimize args) (do (.findOptimalSolution solver ResolutionPolicy/MINIMIZE (eval-constraint-expr (:minimize args)))
                         (swap! n-atom inc)
                         true)
      :else (if (= @n-atom 0)
              (and (.findSolution solver)
                   (swap! n-atom inc)
                   true)
              (and (.nextSolution solver)
                   (swap! n-atom inc)
                   true)))))

(defn solution
  "Solves the solver using the posted constraints and returns a map from variable names to their values (or nil if there is no solution).
You can call this function more than once, getting a new solution each time (like \"solve!\").
Keyword arguments:
- :maximize <var> - finds the solution maximizing the given variable.
- :minimize <var> - finds the solution minimizing the given variable.
When optimizing a variable, if the problem is infeasible, a solution will be found anyway that bypasses some or all of the constraints.
When not optimizing a variable, you can call this function multiple times, which will return new, updated solution maps representing the next solution (if any).

Note: returned solution maps have the metadata {:solution <n>} denoting that it is the nth solution found (starting with 0)."
  [solver & args]
  (let [solved? (apply solve! solver args)]
    (when solved?
      (solution-map solver (dec @(:n-solutions solver))))))

(defn solutions
  "Solves the solver using the posted constraints and returns a lazy seq of maps (for each solution) from variable names to their values.
You shouldn't call this function more than once."
  [solver]
  (take-while identity
              (repeatedly #(solution solver))))