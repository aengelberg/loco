(ns loco.core-test
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basic-test
  (-> (solutions
        [($in :x 1 3)
         ($in :y 1 3 :bounded)
         ($in :z [1 3 2])
         ($= :x :y)
         ($= :y :z)])
    (= '({:x 1 :y 1 :z 1} {:x 2 :y 2 :z 2} {:x 3 :y 3 :z 3}))
    is))

(deftest arithmetic-test
  (-> (solutions
        [($in :x -5 5)
         ($in :y -5 5)
         ($in :z -5 5)
         ($= ($+ :x :y) 5)
         ($= ($- :x :z) 2)
         ($= ($* :y :z) 2)])
    set
    (= #{{:z 1, :y 2, :x 3} {:z 2, :y 1, :x 4}})
    is))

(deftest abs-test
  (-> (solutions
        [($in :x -5 5)
         ($= ($abs :x) 2)])
    set
    (= #{{:x -2} {:x 2}})
    is))

(deftest minmax-test
  (-> (solutions
        [($in :x -5 5)
         ($in :y -5 5)
         ($in :z -5 5)
         ($= ($min :x :y :z) :x)
         ($= ($max :x :y :z) :z)
         ($= :x -5)
         ($= :z -5)])
    (= '({:x -5 :y -5 :z -5}))
    is)
  (-> (solutions
        [($in :x 1 5)
         ($in :y 2 6)
         ($in :z 3 7)
         ($= ($min :x 5) 5)
         ($= ($max :z 3) 3)
         ($= :x :y)])
    (= '({:x 5 :z 3 :y 5}))
    is))

(deftest mod-scalar-test
  (-> (solutions
        [($in :x 1 5)
         ($in :y 1 5)
         ($in :z 1 5)
         ($= ($mod :x :y) 4)
         ($= ($scalar [:x :y :z] '(1 1 -2)) 3)])
    (= '({:x 4 :y 5 :z 3}))
    is))

(deftest eq-ineq-test
  (-> (solutions
        [($in :x 1 5)
         ($in :y 1 5)
         ($in :z 1 5)
         ($= :z 2)
         ($< :x :y)
         ($<= :y :z)
         ($> :y :x)
         ($>= :z :y)
         ($!= :x :y)
         ($!= :x :y :z)])
    (= '({:x 1 :y 2 :z 2}))
    is))

(deftest logic-test
  (-> (solutions
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
           :else ($true))])
    (= '({:x 1}))
    is))

(deftest reify-test
  (-> (solutions
        [($in :x 0 1)
         ($= ($reify ($true)) :x)
         ($= ($reify ($false)) ($- 1 :x))])
    (= '({:x 1}))
    is))

(deftest all-different-test
  (-> (solutions
        [($in :x 0 1)
         ($in :y [1])
         ($in :z 1 2)
         ($distinct [:x :y :z])
         ($not ($distinct [:x :x]))])
    (= '({:x 0 :y 1 :z 2}))
    is))

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
  (-> (solutions
        [($in :a [5])
         ($in :b [5])
         ($in :c [2])
         ($in :d [5])
         ($in :e [5])
         ($in :x 0 4)
         ($= ($nth [:a :b :c :d :e] :x) :x)])
    (as-> s (and (= (count s) 1)
                 (= ((first s) :x) 2)))
    is)
  (-> (solutions
        [($in :a [5])
         ($in :b [5])
         ($in :c [3])
         ($in :d [5])
         ($in :e [5])
         ($in :x 0 4)
         ($= ($nth [:a :b :c :d :e] :x 1) :x)])
    (as-> s (and (= (count s) 1)
                 (= ((first s) :x) 3)))
    is))

(deftest automaton-test
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

(deftest tricky-test-0-2-1
  ; testing ($comparison number var)
  (-> (solutions
        [($in :a [5])
         ($in :b 0 1)
         ($= :b ($reify ($< 0 :a)))])
    (= '({:a 5 :b 1}))
    is))