(ns loco.constraints
  (:require [loco.core :as core :refer [eval-constraint-expr
                                        eval-constraint-expr*
                                        *solver*]])
  (:import (org.chocosolver.solver.constraints Arithmetic
                                               ICF
                                               LCF
                                               Constraint)
           (org.chocosolver.solver.variables IntVar
                                             BoolVar
                                             VF)
           org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

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

(defn make-const-var
  [n]
  (VF/fixed n (:csolver *solver*)))

(defn make-int-var
  [min max]
  (VF/enumerated (str (gensym "_int-var")) min max (:csolver *solver*)))

(defn make-bool-var
  []
  (VF/bool (str (gensym "_bool-var")) (:csolver *solver*)))

(defn- constrain!
  [constraint]
  (.post (:csolver *solver*) constraint))

(defn- to-int-var
  [x]
  (cond
    (number? x) (make-const-var x)
    (instance? IntVar x) x
    (instance? Constraint x) (throw (IllegalArgumentException. "Expected a variable or a number, but got a constraint"))
    :else (throw (IllegalArgumentException. "Constraint expected an int-var or number"))))

(defn- id
  []
  (gensym "id"))

;;;;; VAR GENERATION

(defn $in
  "Declares that a variable must be in a certain domain.
Possible arglist examples:
($in :x 1 5)
($in :x [1 2 3 4 5])
($in :x 1 5 :bounded)"
  [var-name & args]
  {:pre [(or (keyword? var-name)
             (and (vector? var-name)
                  (keyword? (first var-name))))]}
  (let [m {:type :int-domain
           :can-init-var true
           :name var-name}
        [m args] (if (number? (first args))
                   [(assoc m :domain {:min (first args) :max (second args)}) (rest (rest args))]
                   [(assoc m :domain (first args)) (rest args)])
        [m args] (if (and (= (first args) :bounded) (map? (:domain m)))
                   [(assoc-in m [:domain :bounded] true) (rest args)]
                   [m args])]
    (when (and (:bounded m)
               (not (map? (:domain m))))
      (throw (IllegalArgumentException. "Bounded domains take a min and a max only")))
    m))

(defmethod eval-constraint-expr* :int-domain
  [{var-name :name domain :domain}]
  (let [v (eval-constraint-expr var-name)]
    (if (map? domain)
      (ICF/member v (:min domain) (:max domain))
      (ICF/member v (int-array (sort (distinct domain)))))))

(defmethod eval-constraint-expr* :int-var
  [data]
  (let [domain-expr (:domain data)
        var-name (:name data)
        real-name (:real-name data)
        v (case [(boolean (:bounded domain-expr)) (sequential? domain-expr)]
            [false false] (VF/enumerated real-name (:min domain-expr) (:max domain-expr)
                                         (:csolver *solver*))
            [false true] (VF/enumerated real-name (int-array (sort domain-expr))
                                        (:csolver *solver*))
            [true false] (VF/bounded real-name (:min domain-expr) (:max domain-expr)
                                     (:csolver *solver*)))]
    (swap! (:my-vars *solver*) assoc var-name v)
    nil))

(defmethod eval-constraint-expr* :bool-var
  [{var-name :name real-name :real-name}]
  (let [v (VF/bool real-name (:csolver *solver*))]
    (swap! (:my-vars *solver*) assoc var-name v)
    nil))

;;;;; ARITHMETIC

(defn- $+view
  [x const]
  (VF/offset x const))

(defn $+
  "Takes a combination of int-vars and numbers, and returns another number/int-var which is constrained to equal the sum."
  ([x]
    x)
  ([x y & more]
    {:type :+
     :args (list* x y more)
     :id (id)
     :can-optimize-eq (if (empty? more)
                        #{}
                        #{:= :< :> :<= :>= :!=})}))

(defmethod eval-constraint-expr* :+
  [{args :args :as m}]
  (if (:optimizing-eq m)
    (let [args (map eval-constraint-expr args)
          args (map to-int-var args)]
      (ICF/sum (into-array IntVar args)
               (name (:optimizing-eq m))
               (to-int-var (eval-constraint-expr (:eq-arg m)))))
    (let [[x y & more] (map eval-constraint-expr args)]
      (cond
        (and (empty? more)
             (number? y)) ($+view x y)
        (and (empty? more)
             (number? x)) ($+view y x)
        :else (let [vars (list* x y more)
                    vars (map to-int-var vars) ; converting numbers to int-views
                    mins (map #(.getLB ^IntVar %) vars)
                    maxes (map #(.getUB ^IntVar %) vars)
                    sum-var (make-int-var (apply + mins) (apply + maxes))
                    vars (try (into-array BoolVar vars)
                           (catch Exception e
                             (into-array IntVar vars)))
                    ]
                (constrain! (ICF/sum (into-array IntVar vars) sum-var))
                sum-var)))))

(defn $-
  "Takes a combination of int-vars and numbers, and returns another number/int-var which is constrained
to equal (x - y - z - ...) or (-x) if there's only one argument."
  ([x]
    (if (number? x)
      (- x)
      {:type :neg
       :arg x
       :id (id)}))
  ([x & more]
    (apply $+ x (map $- more))))
(defmethod eval-constraint-expr* :neg
  [{x :arg}]
  (let [x (eval-constraint-expr x)]
    (if (number? x)
      (- x)
      (VF/minus x))))

(declare $=)

(defn $*
  "Takes two arguments. One of the arguments can be a number greater than or equal to -1."
  [x y]
  {:type :*
   :arg1 x
   :arg2 y
   :id (id)
   :can-optimize-eq #{:=}})

(defn- $*view
  [x const]
  (VF/scale x const))
(defn- keypoints
  [vars op neutral]
  (if (empty? vars)
    [neutral]
    (let [v (first vars)
          lo (domain-min v)
          hi (domain-max v)]
      (for [arg1 [lo hi]
            arg2 (keypoints (rest vars) op neutral)]
        (op arg1 arg2)))))
(defmethod eval-constraint-expr* :*
  [{x :arg1 y :arg2 eq-arg :eq-arg}]
  (if eq-arg
    (let [x (eval-constraint-expr x)
          y (eval-constraint-expr y)]
      (cond
        (number? y) (eval-constraint-expr ($= ($*view x y) eq-arg))
        (number? x) (eval-constraint-expr ($= ($*view y x) eq-arg))
        :else (ICF/times x y (to-int-var (eval-constraint-expr eq-arg)))))
    (let [x (eval-constraint-expr x)
          y (eval-constraint-expr y)]
      (cond
        (number? y) ($*view x y)
        (number? x) ($*view y x)
        :else (let [nums (keypoints [x y] * 1)
                    total-min (apply min nums)
                    total-max (apply max nums)
                    z (make-int-var total-min total-max)]
                (constrain! (ICF/times x y z))
                z)))))

(defn $min
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers."
  [& args]
  {:type :min
   :args args
   :id (id)
   :can-optimize-eq #{:=}})

(defmethod eval-constraint-expr* :min
  [{args :args eq-arg :eq-arg}]
  (if eq-arg
    (let [args (for [arg args]
                 (to-int-var (eval-constraint-expr arg)))
          eq-arg (eval-constraint-expr eq-arg)
          eq-arg (to-int-var eq-arg)]
      (if (= (count args) 2)
        (ICF/minimum eq-arg (first args) (second args))
        (ICF/minimum eq-arg (into-array IntVar args))))
    (let [args (map eval-constraint-expr args)
          args (map to-int-var args)
          mins (map domain-min args)
          maxes (map domain-max args)
          final-min (apply min mins)
          final-max (apply min maxes)
          new-var (make-int-var final-min final-max)]
      (constrain! (if (= (count args) 2)
                    (ICF/minimum new-var (first args) (second args))
                    (ICF/minimum new-var (into-array IntVar args))))
      new-var)))

(defn $max
  "The maximum of several arguments. The arguments can be a mixture of int-vars and numbers."
  [& args]
  {:type :max
   :args args
   :id (id)
   :can-optimize-eq #{:=}})

(defmethod eval-constraint-expr* :max
  [{args :args eq-arg :eq-arg}]
  (if eq-arg
    (let [args (for [arg args]
                 (to-int-var (eval-constraint-expr arg)))
          eq-arg (eval-constraint-expr eq-arg)
          eq-arg (to-int-var eq-arg)]
      (if (= (count args) 2)
        (ICF/maximum eq-arg (first args) (second args))
        (ICF/maximum eq-arg (into-array IntVar args))))
    (let [args (map #(eval-constraint-expr %) args)
          args (map to-int-var args)
          mins (map domain-min args)
          maxes (map domain-max args)
          final-min (apply max mins)
          final-max (apply max maxes)
          new-var (make-int-var final-min final-max)]
      (constrain! (if (= (count args) 2)
                    (ICF/maximum new-var (first args) (second args))
                    (ICF/maximum new-var (into-array IntVar args))))
      new-var)))

(defn $mod
  "Given variables X and Y, returns X mod Y."
  [X Y]
  {:type :mod
   :arg1 X
   :arg2 Y
   :id (id)
   :can-optimize-eq #{:=}})

(defmethod eval-constraint-expr* :mod
  [{X :arg1 Y :arg2 Z? :eq-arg}]
  (if Z?
    (let [X (eval-constraint-expr X)
          Y (eval-constraint-expr Y)
          Z (eval-constraint-expr Z?)
          X (to-int-var X)
          Y (to-int-var Y)
          Z (to-int-var Z)]
      (ICF/mod X Y Z))
    (let [X (eval-constraint-expr X)
          Y (eval-constraint-expr Y)
          X (to-int-var X)
          Y (to-int-var Y)
          Ymax (domain-max Y)
          Z (make-int-var 0 (max (dec Ymax) 0))]
      (constrain! (ICF/mod X Y Z))
      Z)))

(defn $abs
  "Given a variable X, returns the absolute value of X, or |X|."
  [X]
  {:type :abs
   :arg X
   :id (id)
   :can-optimize-eq #{:=}})

(defmethod eval-constraint-expr* :abs
  [{X :arg Y? :eq-arg}]
  (let [X (eval-constraint-expr X)
        X (to-int-var X)]
    (if Y?
      (let [Y (eval-constraint-expr Y?)
            Y (to-int-var Y)]
        (ICF/absolute Y X))
      (VF/abs X))))

(defn $scalar
  "Given a list of variables X, Y, Z, etc. and a list of number coefficients a, b, c, etc. returns a new variable constrained to equal aX + bY + cZ ..."
  [variables coefficients]
  {:type :scalar
   :variables variables
   :coefficients coefficients
   :id (id)
   :can-optimize-eq #{:=}})

(defmethod eval-constraint-expr* :scalar
  [{variables :variables coefficients :coefficients opt-eq :optimizing-eq eq-arg :eq-arg}]
  (if opt-eq
    (let [variables (map eval-constraint-expr variables)
          variables (map to-int-var variables)]
      (ICF/scalar (into-array IntVar variables)
                  (int-array coefficients)
                  (name opt-eq)
                  (to-int-var (eval-constraint-expr eq-arg))))
    (let [variables (map eval-constraint-expr variables)
          variables (map to-int-var variables)
          minmaxes (for [[v c] (map vector variables coefficients)
                         :let [dmin (* (domain-min v) c)
                               dmax (* (domain-max v) c)]]
                     (if (< dmin dmax)
                       [dmin dmax]
                       [dmax dmin]))
          final-min (apply + (map first minmaxes))
          final-max (apply + (map second minmaxes))
          new-var (make-int-var final-min final-max)]
      (constrain! (ICF/scalar (into-array IntVar variables) (int-array coefficients) new-var))
      new-var)))

(declare $not $and)

(def ^:private eq-converse
  {:= :=
   :< :>
   :> :<
   :<= :>=
   :>= :<=
   :!= :!=})

(defmacro ^:private defn-equality-constraint
  [fnname docstring eqstr]
  `(defn ~fnname
     ~docstring
     ([X# Y#]
       (cond
         (contains? (:can-optimize-eq X#) ~(keyword eqstr))
         (-> X#
           (assoc :optimizing-eq ~(keyword eqstr) :eq-arg Y#)
           (dissoc :can-optimize-eq)
           (dissoc :id))
         
         (contains? (:can-optimize-eq Y#) ~(eq-converse (keyword eqstr)))
         (-> Y#
           (assoc :optimizing-eq ~(eq-converse (keyword eqstr)) :eq-arg X#)
           (dissoc :can-optimize-eq)
           (dissoc :id))
         
         :else
         {:type :arithm-eq
          :eq ~eqstr
          :arg1 X#
          :arg2 Y#}))
     ([X# Y# & more#]
       (apply $and (map (partial apply ~fnname) (partition 2 1 (list* X# Y# more#)))))))

(defmethod eval-constraint-expr* :arithm-eq
  [data]
  (let [op (:eq data)
        X (eval-constraint-expr (:arg1 data))
        Y (eval-constraint-expr (:arg2 data))
        X (to-int-var X)
        Y (to-int-var Y)]
    ;(println X Y)
    (ICF/arithm X (name op) Y)))

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
  "Constrains that X != Y, i.e. (not X = Y = ...)"
  ([X Y]
    {:type :arithm-eq
     :eq :!=
     :arg1 X
     :arg2 Y})
  ([X Y Z & more]
    ($not (apply $= X Y Z more))))

;;;;; LOGIC

(defn $true
  "Always true."
  []
  {:type :true
   :id (id)})
(defmethod eval-constraint-expr* :true
  [_]
  (ICF/TRUE (:csolver *solver*)))
(defn $false
  "Always false."
  []
  {:type :false
   :id (id)})
(defmethod eval-constraint-expr* :false
  [_]
  (ICF/FALSE (:csolver *solver*)))

(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if and only if every subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($true)
    {:type :and, :constraints constraints}))
(defmethod eval-constraint-expr* :and
  [{constraints :constraints}]
  (let [constraints (map eval-constraint-expr constraints)]
    (LCF/and (into-array Constraint constraints))))
(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and only if at least one subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($false)
    {:type :or, :constraints constraints}))
(defmethod eval-constraint-expr* :or
  [{constraints :constraints}]
  (let [constraints (map eval-constraint-expr constraints)]
    (LCF/or (into-array Constraint constraints))))

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  [C]
  {:type :not, :arg C})

(defmethod eval-constraint-expr* :not
  [{C :arg}]
  (LCF/not (eval-constraint-expr C)))

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole statement is false).
An optional \"else\" field can be specified, which must be true if P is false."
  ([if-this then-this]
    {:type :if
     :if if-this
     :then then-this})
  ([if-this then-this else-this]
    {:type :if
     :if if-this
     :then then-this
     :else else-this}))
(defmethod eval-constraint-expr* :if
  [{if-this :if then-this :then else-this :else}]
  (if-not else-this
    (LCF/ifThen_reifiable (eval-constraint-expr if-this)
                          (eval-constraint-expr then-this))
    (LCF/ifThenElse_reifiable (eval-constraint-expr if-this)
                              (eval-constraint-expr then-this)
                              (eval-constraint-expr else-this))))

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
  "Given a constraint C, will generate a bool-var V such that (V = 1) iff C."
  [C]
  {:type :reify
   :arg C
   :id (id)})
(defmethod eval-constraint-expr* :reify
  [{C :arg}]
  (let [C (eval-constraint-expr C)
        V (make-bool-var)]
    (LCF/reification V C)
    V))

;;;;; GLOBAL

(defn $distinct
  "Given a bunch of int-vars, ensures that all of them have different values, i.e. no two of them are equal."
  [vars]
  {:type :distinct
   :args vars})
(defmethod eval-constraint-expr* :distinct
  [{vars :args}]
  (let [vars (map eval-constraint-expr vars)]
    (ICF/alldifferent (into-array IntVar vars) "DEFAULT")))

(defn $all-different?
  "Deprecated: use $distinct"
  [& vars]
  ($distinct vars))

(defn $circuit
  "Given a list of int-vars L, and an optional offset number (default 0), the elements of L define a circuit, where
(L[i] = j + offset) means that j is the successor of i.
Hint: make the offset 1 when using a 1-based list."
  ([list-of-vars]
    ($circuit list-of-vars 0))
  ([list-of-vars offset]
    {:type :circuit
     :vars list-of-vars
     :offset offset}))
(defmethod eval-constraint-expr* :circuit
  [{list-of-vars :vars offset :offset}]
  (let [list-of-vars (map eval-constraint-expr list-of-vars)
        list-of-vars (map to-int-var list-of-vars)]
    (ICF/circuit (into-array IntVar list-of-vars) offset)))

(def $circuit?
  "Deprecated: use $circuit"
  $circuit)

(defn $nth
  "Given a list of int-vars L, an int-var i, and an optional offset number (default 0), returns a new int-var constrained to equal L[i], or L[i - offset]."
  ([list-of-vars index-var]
    ($nth list-of-vars index-var 0))
  ([list-of-vars index-var offset]
    {:type :nth
     :vars list-of-vars
     :index index-var
     :offset offset
     :id (id)
     :can-optimize-eq #{:=}}))
(defmethod eval-constraint-expr* :nth
  [{list-of-vars :vars index-var :index offset :offset eq-arg :eq-arg}]
  (if eq-arg
    (let [list-of-vars (map eval-constraint-expr list-of-vars)
          list-of-vars (map to-int-var list-of-vars)
          index-var (eval-constraint-expr index-var)
          index-var (to-int-var index-var)
          value-var (to-int-var (eval-constraint-expr eq-arg))]
      (ICF/element value-var (into-array IntVar list-of-vars) index-var offset))
    (let [list-of-vars (map eval-constraint-expr list-of-vars)
          list-of-vars (map to-int-var list-of-vars)
          index-var (eval-constraint-expr index-var)
          index-var (to-int-var index-var)
          final-min (apply min (map domain-min list-of-vars))
          final-max (apply max (map domain-max list-of-vars))
          new-var (make-int-var final-min final-max)]
      (constrain! (ICF/element new-var (into-array IntVar list-of-vars) index-var offset))
      new-var)))

(defn $regex
  "Given a regex and a list of variables, constrains that said variables in sequence must satisfy the regex."
  [regex list-of-vars]
  {:type :regex
   :vars list-of-vars
   :auto {:type :automaton :str regex :id (gensym (hash regex))}})
(defmethod eval-constraint-expr* :automaton
  [{regex :str}]
  (FiniteAutomaton. regex))
(defmethod eval-constraint-expr* :regex
  [{list-of-vars :vars auto :auto}]
  (let [list-of-vars (map eval-constraint-expr list-of-vars)
        list-of-vars (map to-int-var list-of-vars)
        auto (eval-constraint-expr auto)]
    (ICF/regular (into-array IntVar list-of-vars)
                 auto)))

(defn $cardinality
  "Takes a list of variables, and a frequency map (from numbers to frequencies), constrains
that the frequency map is accurate. If the :closed flag is set to true, any keys that aren't
in the frequency map can't appear at all in the list of variables.

Example: ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos} :closed true)
=> {:a 1, :b 1, :c 2, :d 2, :e 2
    :ones 2, :twos 3}"
  [variables frequencies & {:as args}]
  {:type :cardinality
   :variables variables
   :values (keys frequencies)
   :occurrences (vals frequencies)
   :closed (:closed args)})
(defmethod eval-constraint-expr* :cardinality
  [{variables :variables values :values occurrences :occurrences closed :closed}]
  (let [values (int-array values)
        occurrences (into-array IntVar (for [v occurrences]
                                         (to-int-var (eval-constraint-expr v))))
        variables (into-array IntVar (for [v variables]
                                       (to-int-var (eval-constraint-expr v))))]
    (ICF/global_cardinality variables values occurrences (boolean closed))))

(defn $knapsack
  "Takes constant weights / values for a list of pre-defined items, and
a list of variables representing the amount of each item. Constrains that
the values of all the items add up to the total-value, while the items'
weights add up to total-weight.

Example: ($knapsack [3 1 2]    ; weights
                    [5 6 7]    ; values
                    [:x :y :z] ; occurrences
                    :W         ; total weight
                    :V)        ; total value"
  [weights values occurrences total-weight total-value]
  (assert (and (every? integer? weights)
               (every? integer? values))
          "$knapsack: weights and values must be collections of constant integers")
  {:type :knapsack
   :weights weights
   :values values
   :occurrences occurrences
   :total-weight total-weight
   :total-value total-value})
(defmethod eval-constraint-expr* :knapsack
  [{:keys [weights values occurrences total-weight total-value]}]
  (let [occurrences (for [v occurrences]
                      (to-int-var (eval-constraint-expr v)))
        total-weight (to-int-var (eval-constraint-expr total-weight))
        total-value (to-int-var (eval-constraint-expr total-value))]
    (ICF/knapsack (into-array IntVar occurrences)
                  ^IntVar total-weight
                  ^IntVar total-value
                  (int-array weights)
                  (int-array values))))
