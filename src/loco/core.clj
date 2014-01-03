(ns loco.core
  (:import (solver.variables VF)
           (solver.exception SolverException)
           solver.ResolutionPolicy
           solver.constraints.Constraint))

(defn- namey?
  [x]
  (try (boolean (name x))
    (catch Exception e
      false)))

(defn- id
  []
  (gensym "id"))

(defmulti eval-constraint-expr*
  "Evaluates a map data structure in the correct behavior, typically returning a constraint or a variable."
  (fn [data solver]
    (or (:type data) (type data))))

(defrecord LocoSolver
  [csolver memo-table my-vars n-solutions])

(defn- solver
  []
  (->LocoSolver
    (solver.Solver. (str (gensym "solver")))
    (atom {})
    (atom {})
    (atom 0)))

(defn- find-int-var
  [solver n]
  (@(:my-vars solver) (name n))) 

(defn- get-val
  [v]
  (.getLB v))

(defn int-var
  [& args]
  (let [m {:type :int-var
           :id (id)}
        [m args] (if (namey? (first args))
                   [(assoc m :name (name (first args))) (rest args)]
                   [(assoc m :name (name (gensym "_int-var"))) args])
        [m args] (if (number? (first args))
                   [(assoc m :domain {:min (first args) :max (second args)}) (rest (rest args))]
                   [(assoc m :domain (first args)) (rest args)])
        [m args] (if (= (first args) :bounded)
                   [(assoc m :bounded true) (rest args)]
                   [m args])]
    (when (and (:bounded m)
               (not (map? (:domain m))))
      (throw (IllegalArgumentException. "Bounded domains take a min and a max only")))
    m))

(defmethod eval-constraint-expr* :int-var
  [data solver]
  (let [domain-expr (:domain data)
        domain-type (if (:bounded data) :bounded :enumerated)
        var-name (:name data)
        v (case [domain-type (sequential? domain-expr)]
            [:enumerated false] (VF/enumerated var-name (:min domain-expr) (:max domain-expr)
                                               (:csolver solver))
            [:enumerated true] (VF/enumerated var-name (int-array (sort domain-expr))
                                              (:csolver solver))
            [:bounded false] (VF/bounded var-name (:min domain-expr) (:max domain-expr)
                                         (:csolver solver)))]
    (swap! (:my-vars solver) assoc var-name v)
    v))

(defn bool-var
  [& args]
  (let [m {:type :bool-var
           :id (id)}
        m (if (namey? (first args))
            (assoc m :name (name (first args)))
            (assoc m :name (name (gensym "_bool-var"))))]
    m))

(defmethod eval-constraint-expr* :bool-var
  [{var-name :name} solver]
  (let [v (VF/bool var-name (:csolver solver))]
    (swap! (:my-vars solver) assoc var-name v)
    v))

(defn eval-constraint-expr
  "Memoized version of eval-constraint-expr*"
  [data solver]
  (let [lookup (when (:id data)
                 (@(:memo-table solver) (:id data)))]
    (if lookup
      lookup
      (let [result (eval-constraint-expr* data solver)]
        (when (:id data)
          (swap! (:memo-table solver) assoc (:id data) result))
        result))))

(defmethod eval-constraint-expr* :default
  [data solver]
  data)

(defmethod eval-constraint-expr* String
  [data solver]
  (find-int-var solver data))

(defmethod eval-constraint-expr* clojure.lang.Keyword
  [data solver]
  (find-int-var solver data))

(defmethod eval-constraint-expr* clojure.lang.Symbol
  [data solver]
  (find-int-var solver data))

(defn- solution-map
  [solver n]
  (into (with-meta {} {:loco/solution n})
        (for [v (vals @(:my-vars solver))
              :let [n (.getName v)]
              :when (not= (first n) \_)]
          [n (get-val v)])))

(defn- constrain!
  [solver constraint]
  (.post (:csolver solver) constraint))

(defn- problem->solver
  [problem]
  (let [s (solver)]
    (doseq [i problem
            :let [i (eval-constraint-expr i s)]]
      (when (instance? Constraint i)
        (constrain! s i)))
    s))

(defn- solve!
  [solver & args]
  (let [args (apply hash-map args)
        n-atom (:n-solutions solver)
        solver (:csolver solver)]
    (cond
      (:maximize args) (do (.findOptimalSolution solver ResolutionPolicy/MAXIMIZE (eval-constraint-expr solver (:maximize args)))
                         (swap! n-atom inc)
                         true)
      (:minimize args) (do (.findOptimalSolution solver ResolutionPolicy/MINIMIZE (eval-constraint-expr solver (:minimize args)))
                         (swap! n-atom inc)
                         true)
      :else (if (= @n-atom 0)
              (and (.findSolution solver)
                   (swap! n-atom inc)
                   true)
              (and (.nextSolution solver)
                   (swap! n-atom inc)
                   true)))))

(defn solution*
  [solver & args]
  (let [solved? (apply solve! solver args)]
    (when solved?
      (solution-map solver (dec @(:n-solutions solver))))))

(defn solution
  "Solves the problem using the specified constraints and returns a map from variable names to their values (or nil if there is no solution).
Keyword arguments:
- :maximize <var> - finds the solution maximizing the given variable.
- :minimize <var> - finds the solution minimizing the given variable.
When optimizing a variable, if the problem is infeasible, a solution will be found anyway that bypasses some or all of the constraints.

Note: returned solution maps have the metadata {:solution <n>} denoting that it is the nth solution found (starting with 0)."
  [problem & args]
  (apply solution* (problem->solver problem) args))

(defn solutions
  "Solves the solver using the constraints and returns a lazy seq of maps (for each solution) from variable names to their values."
  [problem]
  (let [solver (problem->solver problem)]
    (take-while identity
                (repeatedly #(solution* solver)))))