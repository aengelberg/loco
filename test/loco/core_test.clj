(ns loco.core-test
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basic-test
  (let [s (solver)
        x (int-var s "x" 1 3)
        y (int-var s :y 1 3 :bounded)
        z (int-var s 'z [1 3 2])]
    (constrain! s
                ($= x y)
                ($= y z))
    (is (= (solutions s) '({"x" 1 "y" 1 "z" 1} {"x" 2 "y" 2 "z" 2} {"x" 3 "y" 3 "z" 3})))))

(deftest arithmetic-test
  (let [s (solver)
        x (int-var s "x" -5 5)
        y (int-var s "y" -5 5)
        z (int-var s "z" -5 5)]
    (constrain! s
                ($= ($+ x :y) 5)
                ($= ($- :x z) 2)
                ($= ($* :y :z) 2))
    (is (= (solutions s) '({"z" 1, "y" 2, "x" 3} {"z" 2, "y" 1, "x" 4})))))

(deftest minmax-test
  (-> (solver)
    (doto
      (int-var "x" -5 5)
      (int-var "y" -5 5)
      (int-var "z" -5 5)
      (constrain! ($= ($min :x :y :z) :x)
                  ($= ($max :x :y :z) :z)
                  ($= :x -5)
                  ($= :z -5)))
    (solutions)
    (= '({"x" -5 "y" -5 "z" -5}))
    is)
  (-> (solver)
    (doto
      (int-var "x" 1 5)
      (int-var "y" 2 6)
      (int-var "z" 3 7)
      (constrain! ($= ($min :x 5) 5)
                  ($= ($max :z 3) 3)
                  ($= :x :y)))
    (solutions)
    (= '({"x" 5 "z" 3 "y" 5}))
    is))

(deftest mod-scalar-test
  (-> (solver)
    (doto
      (int-var "x" 1 5)
      (int-var "y" 1 5)
      (int-var "z" 1 5)
      (constrain! ($= ($mod :x :y) 4))
      (constrain! ($= ($scalar [:x :y :z] '(1 1 -2)) 3)))
    (solutions)
    (= '({"x" 4 "y" 5 "z" 3}))
    is))

(deftest eq-ineq-test
  (-> (solver)
    (doto
      (int-var "x" 1 5)
      (int-var "y" 1 5)
      (int-var "z" 1 5)
      (constrain! ($= :z 2)
                  ($< :x :y)
                  ($<= :y :z)
                  ($> :y :x)
                  ($>= :z :y)
                  ($!= :x :y)
                  ($!= :x :y :z)))
    (solutions)
    (= '({"x" 1 "y" 2 "z" 2}))
    is))

(deftest logic-test
  (-> (solver)
    (doto
      (int-var "x" [1])
      (constrain! ($true)
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
                    :else ($true)))
      (solutions)
      (= '({"x" 1}))
      is)))

(deftest reify-test
  (-> (solver)
    (doto
      (int-var "x" 0 1)
      (constrain! ($= ($reify ($true)) :x)
                  ($= ($reify ($false)) ($- 1 :x))))
    (solutions)
    (= '({"x" 1}))
    is))

(deftest all-different-test
  (-> (solver)
    (doto
      (int-var "x" 0 1)
      (int-var "y" [1])
      (int-var "z" 1 2)
      (constrain! ($all-different? :x :y :z)
                  ($not ($all-different? :x :x))))
    (solutions)
    (= '({"x" 0 "y" 1 "z" 2}))
    is))

(deftest circuit-test
  (-> (solver)
    (doto
      (int-var "a" 0 4)
      (int-var "b" [0])
      (int-var "c" 0 4)
      (int-var "d" 0 4)
      (int-var "e" 0 4)
      (constrain! ($circuit? [:a :b :c :d :e])))
    (solution)
    (as-> sol
          (let [a ["a" "b" "c" "d" "e"]
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
  (-> (solver)
    (doto
      (int-var "a" 1 5)
      (int-var "b" [1])
      (int-var "c" 1 5)
      (int-var "d" 1 5)
      (int-var "e" 1 5)
      (constrain! ($circuit? [:a :b :c :d :e] 1)))
    (solution)
    (as-> sol
          (let [a ["a" "b" "c" "d" "e"]
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
  (-> (solver)
    (doto
      (int-var "a" [5])
      (int-var "b" [5])
      (int-var "c" [2])
      (int-var "d" [5])
      (int-var "e" [5])
      (int-var "x" 0 4)
      (constrain! ($= ($nth [:a :b :c :d :e] :x) :x)))
    (solutions)
    (as-> s (and (= (count s) 1)
                 (= ((first s) "x") 2)))
    is)
  (-> (solver)
    (doto
      (int-var "a" [5])
      (int-var "b" [5])
      (int-var "c" [3])
      (int-var "d" [5])
      (int-var "e" [5])
      (int-var "x" 0 4)
      (constrain! ($= ($nth [:a :b :c :d :e] :x 1) :x)))
    (solutions)
    (as-> s (and (= (count s) 1)
                 (= ((first s) "x") 3)))
    is))

(deftest automaton-test
  (let [a (automaton "(1|2)3*(4|5)")]
    (-> (solver)
      (doto
        (int-var "a" [1])
        (int-var "b" [2])
        (int-var "c" [3])
        (int-var "d" [4])
        (int-var "e" [5])
        (constrain! ($satisfies-automaton? a [:a :d])
                    ($satisfies-automaton? a [:a :c :c :c :d])
                    ($not ($satisfies-automaton? a [:a :b :c :c :c :d]))))
      (solutions)
      count
      (= 1)
      is)))