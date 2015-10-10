(ns loco.core-test
  (:use clojure.test
        loco.core
        loco.constraints)
  (:require [loco.automata :as a]))

(defmacro test-constraint-model
  ([docstring model solution-maps]
   `(testing ~docstring
      (is (= ~(set solution-maps)
             (set (solutions ~model))))))
  ([model solution-maps]
   `(test-constraint-model nil ~model ~solution-maps)))

(deftest basic-test
  (test-constraint-model
   "Basic test case with $= and bounded vars "
   [($in :x 1 3)
    ($in :y 1 3 :bounded)
    ($in :z [1 3 2])
    ($= :x :y)
    ($= :y :z)]
   [{:x 1 :y 1 :z 1} {:x 2 :y 2 :z 2} {:x 3 :y 3 :z 3}]))

(deftest arithmetic-test
  (test-constraint-model
   [($in :x -5 5)
    ($in :y -5 5)
    ($in :z -5 5)
    ($= ($+ :x :y) 5)
    ($= ($- :x :z) 2)
    ($= ($* :y :z) 2)]
   [{:z 1, :y 2, :x 3} {:z 2, :y 1, :x 4}]))

(deftest abs-test
  (test-constraint-model
   [($in :x -5 5)
    ($= ($abs :x) 2)]
   [{:x -2} {:x 2}]))

(deftest minmax-test
  (test-constraint-model
   [($in :x -5 5)
    ($in :y -5 5)
    ($in :z -5 5)
    ($= ($min :x :y :z) :x)
    ($= ($max :x :y :z) :z)
    ($= :x -5)
    ($= :z -5)]
   [{:x -5 :y -5 :z -5}])
  (test-constraint-model
   [($in :x 1 5)
    ($in :y 2 6)
    ($in :z 3 7)
    ($= ($min :x 5) 5)
    ($= ($max :z 3) 3)
    ($= :x :y)]
   [{:x 5 :z 3 :y 5}]))

(deftest mod-scalar-test
  (test-constraint-model
   [($in :x 1 5)
    ($in :y 1 5)
    ($in :z 1 5)
    ($= ($mod :x :y) 4)
    ($= ($scalar [:x :y :z] '(1 1 -2)) 3)]
   [{:x 4 :y 5 :z 3}]))

(deftest eq-ineq-test
  (test-constraint-model
   [($in :x 1 5)
    ($in :y 1 5)
    ($in :z 1 5)
    ($= :z 2)
    ($< :x :y)
    ($<= :y :z)
    ($> :y :x)
    ($>= :z :y)
    ($!= :x :y)
    ($!= :x :y :z)]
   [{:x 1 :y 2 :z 2}]))

(deftest logic-test
  (test-constraint-model
   [($in :x [1])
    ($true)
    ($not ($false))
    ($not ($not ($true)))
    ($and ($true) ($true))
    ($not ($and ($true) ($false)))
    ($or ($true) ($false))
    ($if ($true) ($true) ($false))
    ($if ($false) ($false))
    ($if ($false) ($false) ($true))
    ($cond
     ($false) ($true)
     ($false) ($false)
     ($true) ($true)
     ($false) ($true)
     :else ($true))]
    [{:x 1}]))

(deftest reify-test
  (test-constraint-model
   [($in :x 0 1)
    ($= ($reify ($true)) :x)
    ($= ($reify ($false)) ($- 1 :x))]
   [{:x 1}]))

(deftest all-different-test
  (test-constraint-model
   [($in :x 0 1)
    ($in :y [1])
    ($in :z 1 2)
    ($distinct [:x :y :z])
    ($not ($distinct [:x :x]))]
   [{:x 0 :y 1 :z 2}]))

(deftest circuit-test
  (-> (solution
        [($in :a 0 4)
         ($in :b [0])
         ($in :c 0 4)
         ($in :d 0 4)
         ($in :e 0 4)
         ($circuit [:a :b :c :d :e])])
    (as-> sol
          (let [a [:a :b :c :d :e]
                [v i] (first sol)
                w (a i)
                i (sol w)
                x (a i)
                i (sol x)
                y (a i)
                i (sol y)
                z (a i)]
            (is (= (count (distinct [v w x y z])) 5)))))
  ;testing offset
  (-> (solution
        [($in :a 1 5)
         ($in :b [1])
         ($in :c 1 5)
         ($in :d 1 5)
         ($in :e 1 5)
         ($circuit [:a :b :c :d :e] 1)])
    (as-> sol
          (let [a [:a :b :c :d :e]
                [v i] (first sol)
                w (a (dec i))
                i (sol w)
                x (a (dec i))
                i (sol x)
                y (a (dec i))
                i (sol y)
                z (a (dec i))]
            (is (= (count (distinct [v w x y z])) 5))))))

(deftest nth-test
  (test-constraint-model
   [($in :a [5])
    ($in :b [5])
    ($in :c [2])
    ($in :d [5])
    ($in :e [5])
    ($in :x 0 4)
    ($= ($nth [:a :b :c :d :e] :x) :x)]
   [{:a 5 :b 5 :c 2 :d 5 :e 5 :x 2}])
  (test-constraint-model
   [($in :a [5])
    ($in :b [5])
    ($in :c [3])
    ($in :d [5])
    ($in :e [5])
    ($in :x 0 4)
    ($= ($nth [:a :b :c :d :e] :x 1) :x)]
   [{:a 5 :b 5 :c 3 :d 5 :e 5 :x 3}]))

(deftest deprecated-regex-test
  (let [regex "(1|2)3*(4|5)"]
    (-> (solutions
          [($in :a [1])
           ($in :b [2])
           ($in :c [3])
           ($in :d [4])
           ($in :e [5])
           ($regex regex [:a :d])
           ($regex regex [:a :c :c :c :d])
           ($not ($regex regex [:a :b :c :c :c :d]))])
      count
      (= 1)
      is)))

(deftest automaton-test
  (doseq [[description automaton]
          [["string->automaton"
            (a/string->automaton "12*3+")]
           ["map->automaton"
            (a/map->automaton {:q0 {1 :q1}
                               :q1 {2 :q1
                                    3 :q2}
                               :q2 {3 :q2}}
                              :q0 #{:q2})]
           ["make-automaton"
            (a/make-automaton [:q0 :q1 :q2]
                              [[:q0 :q1 1]
                               [:q1 :q1 2]
                               [:q1 :q2 3]
                               [:q2 :q2 3]]
                              :q0 [:q2])]
           ["Union two automata"
            (a/union (a/string->automaton "12?3+")
                     (a/string->automaton "12+3+"))]
           ["Concatenate automata"
            (reduce a/cat
                    [(a/string->automaton "1")
                     (a/string->automaton "2*")
                     (a/string->automaton "3+")])]
           ["Intersect automata"
            (a/intersection (a/string->automaton "12*4*3+")
                            (a/string->automaton "12*5?3+"))]]]
    (testing description
      (are [x y] (= (set y) (set (solutions x)))
        [($in :x 1 5)
         ($in :y 1 5)
         ($in :z 1 5)
         ($regular automaton [:x :y :z])]
        '({:x 1 :y 2 :z 3}
          {:x 1 :y 3 :z 3})

        [($in :x 1 5)
         ($regular automaton [:x])]
        ())
      (are [x] (= '({}) (solutions [($regular automaton x)]))
        [1 3]
        [1 2 3]
        [1 2 2 3]
        [1 3 3]
        [1 2 3 3]
        [1 2 2 3 3])
      (are [x] (empty? (solutions [($regular automaton x)]))
        [1]
        ;; [] ; doesn't work, see https://github.com/chocoteam/choco3/issues/335
        [1 3 2]
        [1 2]
        [1 2 2]
        [2 2 3 3]))))

(deftest cardinality-test
  (-> (solutions
        [($in :a 1 5)
         ($in :b 1 5)
         ($in :c 1 5)
         ($in :d 1 5)
         ($in :e 1 5)
         ($in :ones 1 5)
         ($in :twos 1 5)
         ($cardinality [:a :b :c :d :e] {1 :ones 2 :twos})])
    (as-> ms
          (doseq [m ms]
            (-> m
              (map [:a :b :c :d :e])
              frequencies
              (map [1 2])
              (= (map m [:ones :twos]))
              is)))))

(deftest optimization-test
  (is (= (solution [($in :x 1 5)]
                   :maximize :x)
         {:x 5}))
  (is (= (solution [($in :x 1 5)]
                   :minimize :x)
         {:x 1}))
  (is (= (solution [($in :x 1 5) ($< :x 0)]
                   :maximize :x)
         nil))
  (is (= (solution [($in :x 1 5) ($< :x 0)]
                   :minimize :x)
         nil)))

(deftest tricky-test-0-2-1
  "Target specific bug fixed by 0.2.1"
  (test-constraint-model
   [($in :a [5])
    ($in :b 0 1)
    ($= :b ($reify ($< 0 :a)))]
   [{:a 5 :b 1}]))

(deftest knapsack-test
  (test-constraint-model
   "Super basic knapsack constraint"
   [($knapsack [1 1] [1 1] [1 1] 2 2)]
   [{}])
  (test-constraint-model
   "Basic knapsack constraint"
   [($knapsack [2 3]
               [6 5]
               [:x :y]
               :total-weight
               :total-value)
    ($in :x 0 10)
    ($in :y 0 10)
    ($in :total-weight 0 10)
    ($in :total-value 0 10)]
   [{:x 0 :y 0 :total-weight 0 :total-value 0}
    {:x 1 :y 0 :total-weight 2 :total-value 6}
    {:x 0 :y 1 :total-weight 3 :total-value 5}
    {:x 0 :y 2 :total-weight 6 :total-value 10}])
  (test-constraint-model
   "Knapsack constraint with negatives"
   [($in :x -100 100)
    ($in :y -100 100)
    ($knapsack [1 1] [10 20] [:x :y] 10 10)]
   [{:x 19 :y -9}]))
