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
    (if (and (vector? data)
             (keyword? (first data)))
      :vector-var-name
      (or (:type data) (type data)))))

(defn- var-declarations
  [data]
  (filter :var-declaration data))

(defn without-var-declarations
  [data]
  (remove :var-declaration data))

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
  (or (@(:my-vars solver) n)
      (throw (IllegalAccessException. (str "var with name \"" n
                                           "\" is referenced to,"
                                           " but not declared "
                                           "anywhere in the problem")))))

(defn- get-val
  [v]
  (.getLB v))

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

(defmethod eval-constraint-expr* clojure.lang.Keyword
  [data solver]
  (find-int-var solver data))

(defmethod eval-constraint-expr* :vector-var-name
  [data solver]
  (find-int-var solver data))

(defn- solution-map
  [solver n]
  (into (with-meta {} {:loco/solution n})
        (for [[var-name v] @(:my-vars solver)
              :when (if (keyword? var-name)
                      (not= (first (name var-name)) \_)
                      (not= (first (name (first var-name))) \_))]
          [var-name (get-val v)])))

(defn- constrain!
  [solver constraint]
  (.post (:csolver solver) constraint))

(defn- problem->solver
  [problem]
  (let [problem (concat (var-declarations problem)
                        (without-var-declarations problem)) ; dig for the var declarations and put them at the front
        s (solver)]
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