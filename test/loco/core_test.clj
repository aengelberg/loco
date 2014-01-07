(ns loco.core-test
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basic-test
  (-> (solutions
        [($int :x 1 3)
         ($int :y 1 3 :bounded)
         ($int :z [1 3 2])
         ($= :x :y)
         ($= :y :z)])
    (= '({:x 1 :y 1 :z 1} {:x 2 :y 2 :z 2} {:x 3 :y 3 :z 3}))
    is))

(deftest arithmetic-test
  (-> (solutions
        [($int :x -5 5)
         ($int :y -5 5)
         ($int :z -5 5)
         ($= ($+ :x :y) 5)
         ($= ($- :x :z) 2)
         ($= ($* :y :z) 2)])
    set
    (= #{{:z 1, :y 2, :x 3} {:z 2, :y 1, :x 4}})
    is))

(deftest minmax-test
  (-> (solutions
        [($int :x -5 5)
         ($int :y -5 5)
         ($int :z -5 5)
         ($= ($min :x :y :z) :x)
         ($= ($max :x :y :z) :z)
         ($= :x -5)
         ($= :z -5)])
    (= '({:x -5 :y -5 :z -5}))
    is)
  (-> (solutions
        [($int :x 1 5)
         ($int :y 2 6)
         ($int :z 3 7)
         ($= ($min :x 5) 5)
         ($= ($max :z 3) 3)
         ($= :x :y)])
    (= '({:x 5 :z 3 :y 5}))
    is))

(deftest mod-scalar-test
  (-> (solutions
        [($int :x 1 5)
         ($int :y 1 5)
         ($int :z 1 5)
         ($= ($mod :x :y) 4)
         ($= ($scalar [:x :y :z] '(1 1 -2)) 3)])
    (= '({:x 4 :y 5 :z 3}))
    is))

(deftest eq-ineq-test
  (-> (solutions
        [($int :x 1 5)
         ($int :y 1 5)
         ($int :z 1 5)
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
        [($int :x [1])
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
        [($int :x 0 1)
         ($= ($reify ($true)) :x)
         ($= ($reify ($false)) ($- 1 :x))])
    (= '({:x 1}))
    is))

(deftest all-different-test
  (-> (solutions
        [($int :x 0 1)
         ($int :y [1])
         ($int :z 1 2)
         ($all-different? :x :y :z)
         ($not ($all-different? :x :x))])
    (= '({:x 0 :y 1 :z 2}))
    is))

(deftest circuit-test
  (-> (solution
        [($int :a 0 4)
         ($int :b [0])
         ($int :c 0 4)
         ($int :d 0 4)
         ($int :e 0 4)
         ($circuit? [:a :b :c :d :e])])
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
        [($int :a 1 5)
         ($int :b [1])
         ($int :c 1 5)
         ($int :d 1 5)
         ($int :e 1 5)
         ($circuit? [:a :b :c :d :e] 1)])
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
        [($int :a [5])
         ($int :b [5])
         ($int :c [2])
         ($int :d [5])
         ($int :e [5])
         ($int :x 0 4)
         ($= ($nth [:a :b :c :d :e] :x) :x)])
    (as-> s (and (= (count s) 1)
                 (= ((first s) :x) 2)))
    is)
  (-> (solutions
        [($int :a [5])
         ($int :b [5])
         ($int :c [3])
         ($int :d [5])
         ($int :e [5])
         ($int :x 0 4)
         ($= ($nth [:a :b :c :d :e] :x 1) :x)])
    (as-> s (and (= (count s) 1)
                 (= ((first s) :x) 3)))
    is))

(deftest automaton-test
  (let [a (automaton "(1|2)3*(4|5)")]
    (-> (solutions
          [($int :a [1])
           ($int :b [2])
           ($int :c [3])
           ($int :d [4])
           ($int :e [5])
           ($satisfies-automaton? a [:a :d])
           ($satisfies-automaton? a [:a :c :c :c :d])
           ($not ($satisfies-automaton? a [:a :b :c :c :c :d]))])
      count
      (= 1)
      is)))