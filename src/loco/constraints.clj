(ns loco.constraints
  (:require [loco.core :as core :refer [eval-constraint-expr
                                        eval-constraint-expr*]])
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

(defn make-const-var
  [solver n]
  (VF/fixed n (:csolver solver)))

(defn make-int-var
  [solver min max]
  (VF/enumerated (str (gensym "_int-var")) min max (:csolver solver)))

(defn make-bool-var
  [solver]
  (VF/bool (str (gensym "_bool-var")) (:csolver solver)))

(defn- constrain!
  [solver constraint]
  (.post (:csolver solver) constraint))

(defn- to-int-var
  [solver x]
  (cond
    (number? x) (make-const-var solver x)
    (instance? IntVar x) x
    :else (throw (IllegalArgumentException. "Expected int-var or number"))))

(defn- id
  []
  (gensym "id"))

;;;;; VAR GENERATION

(defn $int
  "Declares the domain of a variable. This constraint cannot be nested inside logical constraints.
Possible arglist examples:
($int :x 1 5)
($int :x [1 2 3 4 5])
($int :x 1 5 :bounded)"
  [var-name & args]
  {:pre [(or (keyword? var-name)
             (and (vector? var-name)
                  (keyword? (first var-name))))]}
  (let [m {:type :int-var
           :var-declaration true
           :name var-name
           :real-name (str (gensym "int_var"))}
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
  (when (@(:my-vars solver) (:name data))
    (throw (IllegalArgumentException. (str "var with name " (:name data) " is declared twice"))))
  (let [domain-expr (:domain data)
        domain-type (if (:bounded data) :bounded :enumerated)
        var-name (:name data)
        real-name (:real-name data)
        v (case [domain-type (sequential? domain-expr)]
            [:enumerated false] (VF/enumerated real-name (:min domain-expr) (:max domain-expr)
                                               (:csolver solver))
            [:enumerated true] (VF/enumerated real-name (int-array (sort domain-expr))
                                              (:csolver solver))
            [:bounded false] (VF/bounded real-name (:min domain-expr) (:max domain-expr)
                                         (:csolver solver)))]
    (swap! (:my-vars solver) assoc var-name v)
    nil))

(defn $bool
  "Declares that the domain of a variable is [0;1]. This constraint cannot be nested inside logical constraints."
  [var-name]
  {:pre [(or (keyword? var-name)
             (and (vector? var-name)
                  (keyword? (first var-name))))]}
  {:type :bool-var
   :var-declaration true
   :name var-name
   :real-name (str (gensym "bool_var"))})

(defmethod eval-constraint-expr* :bool-var
  [{var-name :name real-name :real-name} solver]
  (let [v (VF/bool real-name (:csolver solver))]
    (swap! (:my-vars solver) assoc var-name v)
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
     :id (id)}))
(defmethod eval-constraint-expr* :+
  [{args :args} solver]
  (let [[x y & more] (map #(eval-constraint-expr % solver) args)]
    (cond
      (and (empty? more)
           (number? y)) ($+view x y)
      (and (empty? more)
           (number? x)) ($+view y x)
      :else (let [vars (list* x y more)
                  vars (map (partial to-int-var solver) vars) ; converting numbers to int-views
                  mins (map #(.getLB ^IntVar %) vars)
                  maxes (map #(.getUB ^IntVar %) vars)
                  sum-var (make-int-var solver (apply + mins) (apply + maxes))
                  ]
              (constrain! solver (ICF/sum (into-array IntVar vars) sum-var))
              sum-var))))

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
  [{x :arg} solver]
  (let [x (eval-constraint-expr x solver)]
    (if (number? x)
      (- x)
      (VF/minus x))))

(defn $*
  "Takes two arguments. One of the arguments can be a number greater than or equal to -1."
  [x y]
  {:type :*
   :arg1 x
   :arg2 y
   :id (id)})

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
  [{x :arg1 y :arg2} solver]
  (let [x (eval-constraint-expr x solver)
        y (eval-constraint-expr y solver)]
    (cond
      (number? y) ($*view x y)
      (number? x) ($*view y x)
      :else (let [nums (keypoints [x y] * 1)
                  total-min (apply min nums)
                  total-max (apply max nums)
                  z (make-int-var solver total-min total-max)]
              (constrain! solver (ICF/times x y z))
              z))))

(defn $min
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers."
  [& args]
  {:type :min
   :args args
   :id (id)})

(defmethod eval-constraint-expr* :min
  [{args :args} solver]
  (let [args (map #(eval-constraint-expr % solver) args)
        args (map (partial to-int-var solver) args)
        mins (map domain-min args)
        maxes (map domain-max args)
        final-min (apply min mins)
        final-max (apply min maxes)
        new-var (make-int-var solver final-min final-max)]
    (constrain! solver
      (if (= (count args) 2)
        (ICF/minimum new-var (first args) (second args))
        (ICF/minimum new-var (into-array IntVar args))))
    new-var))

(defn $max
  "The maximum of several arguments. The arguments can be a mixture of int-vars and numbers."
  [& args]
  {:type :max
   :args args
   :id (id)})

(defmethod eval-constraint-expr* :max
  [{args :args} solver]
  (let [args (map #(eval-constraint-expr % solver) args)
        args (map (partial to-int-var solver) args)
        mins (map domain-min args)
        maxes (map domain-max args)
        final-min (apply max mins)
        final-max (apply max maxes)
        new-var (make-int-var solver final-min final-max)]
    (constrain! solver
      (if (= (count args) 2)
        (ICF/maximum new-var (first args) (second args))
        (ICF/maximum new-var (into-array IntVar args))))
    new-var))

(defn $mod
  "Given variables X and Y, returns X mod Y."
  [X Y]
  {:type :mod
   :arg1 X
   :arg2 Y
   :id (id)})

(defmethod eval-constraint-expr* :mod
  [{X :arg1 Y :arg2} solver]
  (let [X (eval-constraint-expr X solver)
        Y (eval-constraint-expr Y solver)
        X (to-int-var solver X)
        Y (to-int-var solver Y)
        Ymax (domain-max Y)
        Z (make-int-var solver 0 (max (dec Ymax) 0))]
    (constrain! solver (ICF/mod X Y Z))
    Z))

(defn $scalar
  "Given a list of variables X, Y, Z, etc. and a list of number coefficients a, b, c, etc. returns a new variable constrained to equal aX + bY + cZ ..."
  [variables coefficients]
  {:type :scalar
   :variables variables
   :coefficients coefficients
   :id (id)})

(defmethod eval-constraint-expr* :scalar
  [{variables :variables coefficients :coefficients} solver]
  (let [variables (map #(eval-constraint-expr % solver) variables)
        minmaxes (for [[v c] (map vector variables coefficients)
                       :let [dmin (* (domain-min v) c)
                             dmax (* (domain-max v) c)]]
                   (if (< dmin dmax)
                     [dmin dmax]
                     [dmax dmin]))
        final-min (apply min (map first minmaxes))
        final-max (apply max (map second minmaxes))
        new-var (make-int-var solver final-min final-max)]
    (constrain! solver (ICF/scalar (into-array IntVar variables) (int-array coefficients) new-var))
    new-var))

(declare $not $and)

(defmacro ^:private defn-equality-constraint
  [fnname docstring eqstr]
  `(defn ~fnname
     ~docstring
     ([X# Y#]
       {:type :arithm-eq
        :eq ~eqstr
        :arg1 X#
        :arg2 Y#
        :id (id)})
     ([X# Y# & more#]
       (apply $and (map (partial apply ~fnname) (partition 2 1 (list* X# Y# more#)))))))

(defmethod eval-constraint-expr* :arithm-eq
  [data solver]
  (let [op (:eq data)
        X (eval-constraint-expr (:arg1 data) solver)
        Y (eval-constraint-expr (:arg2 data) solver)
        [X Y] (if (number? X)
                [Y X]
                [X Y])]
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
     :arg2 Y
     :id (id)})
  ([X Y Z & more]
    ($not (apply $= X Y Z more))))

;;;;; LOGIC

(defn $true
  "Always true."
  []
  {:type :true
   :id (id)})
(defmethod eval-constraint-expr* :true
  [_ solver]
  (ICF/TRUE (:csolver solver)))
(defn $false
  "Always false."
  []
  {:type :false
   :id (id)})
(defmethod eval-constraint-expr* :false
  [_ solver]
  (ICF/FALSE (:csolver solver)))

(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if and only if every subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($true)
    {:type :and, :constraints constraints, :id (id)}))
(defmethod eval-constraint-expr* :and
  [{constraints :constraints} solver]
  (let [constraints (map #(eval-constraint-expr % solver) constraints)]
    (LCF/and (into-array Constraint constraints))))
(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and only if at least one subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($false)
    {:type :or, :constraints constraints, :id (id)}))
(defmethod eval-constraint-expr* :or
  [{constraints :constraints} solver]
  (let [constraints (map #(eval-constraint-expr % solver) constraints)]
    (LCF/or (into-array Constraint constraints))))

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  [C]
  {:type :not, :arg C, :id (id)})

(defmethod eval-constraint-expr* :not
  [{C :arg} solver]
  (LCF/not (eval-constraint-expr C solver)))

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole statement is false).
An optional \"else\" field can be specified, which must be true if P is false."
  ([if-this then-this]
    {:type :if
     :if if-this
     :then then-this
     :id (id)})
  ([if-this then-this else-this]
    {:type :if
     :if if-this
     :then then-this
     :else else-this
     :id (id)}))
(defmethod eval-constraint-expr* :if
  [{if-this :if then-this :then else-this :else} solver]
  (if-not else-this
    (LCF/ifThen (eval-constraint-expr if-this solver) (eval-constraint-expr then-this solver))
    (LCF/ifThenElse (eval-constraint-expr if-this solver)
                    (eval-constraint-expr then-this solver)
                    (eval-constraint-expr else-this solver))))

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
  [{C :arg} solver]
  (let [C (eval-constraint-expr C solver)
        V (make-bool-var solver)]
    (constrain! solver (LCF/reification V C))
    V))

;;;;; GLOBAL

(defn $all-different?
  "Given a bunch of int-vars, ensures that all of them have different values, i.e. no two of them are equal."
  [& vars]
  {:type :all-different?
   :args vars
   :id (id)})
(defmethod eval-constraint-expr* :all-different?
  [{vars :args} solver]
  (let [vars (map #(eval-constraint-expr % solver) vars)]
    (ICF/alldifferent (into-array IntVar vars) "DEFAULT")))

(defn $circuit?
  "Given a list of int-vars L, and an optional offset number (default 0), the elements of L define a circuit, where
(L[i] = j + offset) means that j is the successor of i.
Hint: make the offset 1 when using a 1-based list."
  ([list-of-vars]
    ($circuit? list-of-vars 0))
  ([list-of-vars offset]
    {:type :circuit?
     :vars list-of-vars
     :offset offset
     :id (id)}))
(defmethod eval-constraint-expr* :circuit?
  [{list-of-vars :vars offset :offset} solver]
  (let [list-of-vars (map #(eval-constraint-expr % solver) list-of-vars)
        list-of-vars (map (partial to-int-var solver) list-of-vars)]
    (ICF/circuit (into-array IntVar list-of-vars) offset)))

(defn $nth
  "Given a list of int-vars L, an int-var i, and an optional offset number (default 0), returns a new int-var constrained to equal L[i], or L[i - offset]."
  ([list-of-vars index-var]
    ($nth list-of-vars index-var 0))
  ([list-of-vars index-var offset]
    {:type :nth
     :vars list-of-vars
     :index index-var
     :offset offset
     :id (id)}))
(defmethod eval-constraint-expr* :nth
  [{list-of-vars :vars index-var :index offset :offset} solver]
  (let [list-of-vars (map #(eval-constraint-expr % solver) list-of-vars)
        list-of-vars (map (partial to-int-var solver) list-of-vars)
        index-var (eval-constraint-expr index-var solver)
        final-min (apply min (map domain-min list-of-vars))
        final-max (apply max (map domain-max list-of-vars))
        new-var (make-int-var solver final-min final-max)]
    (constrain! solver (ICF/element new-var (into-array IntVar list-of-vars) index-var offset))
    new-var))

(defn automaton
  "Returns a Finite Automaton based on a supplied regular expression, with characters as the non-terminals.
Example of regexp: (1|2)(3*)(4|5)
Numbers (from 0 to 9) are treated as the numbers themselves; non-digit characters are converted to their unicode numbers.
If you wanted to use the number 10 (which would normally be read as 1, 0), you'd have to type \\u000A (hexadecimal for 10).
Be careful to not use spaces; they'll be treated as their ASCII value 32!"
  [regexp]
  (FiniteAutomaton. regexp))

(defn $satisfies-automaton?
  "Given a list of variables and an automaton (created with $automaton), constrains that the list of variables in succession results in a final state in the automaton."
  [automaton list-of-vars]
  {:type :satisfies-automaton?
   :vars list-of-vars
   :automaton automaton
   :id (id)})
(defmethod eval-constraint-expr* :satisfies-automaton?
  [{list-of-vars :vars automaton :automaton} solver]
  (let [list-of-vars (map #(eval-constraint-expr % solver) list-of-vars)
        list-of-vars (map (partial to-int-var solver) list-of-vars)]
    (ICF/regular (into-array IntVar list-of-vars) automaton)))