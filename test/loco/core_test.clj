(ns loco.core-test
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basic-loco-test
  (with-solver (solver "basic_loco")
    (let [x (int-var "x" 1 5)
          y (int-var "y" 5 10)
          z (int-var "z" 10 15)]
      (constrain! ($= ($+ x 3) y)
                  ($= ($+ y 2) z))
      (is (= (solutions) '({:solution 0, "z" 10, "y" 8, "x" 5})))))
  (with-solver (solver "loco_unsolvable")
    (let [x (int-var "x" 1 3)
          y (int-var "y" 4 6)]
      (constrain! ($= x y))
      (is (= (solution) nil)))))

(deftest loco-optimize-test
  (do
    (with-solver (solver "loco_min")
      (let [x (int-var "x" 4 5)
            y (int-var "y" 4 5)]
        (constrain! ($!= x y))
        (is (= (solution :minimize y) {:solution 0, "x" 5, "y" 4}))))
    (with-solver (solver "loco_max")
      (let [x (int-var "x" 4 5)
            y (int-var "y" 4 5)]
        (constrain! ($!= x y))
        (is (= (solution :maximize y) {:solution 0, "x" 4, "y" 5}))))
    (with-solver (solver "loco_min_unsolvable")
      (let [x (int-var "x" 4 4)
            y (int-var "y" 4 4)]
        (constrain! ($!= x y))
        (is (= (solution :minimize y) {:solution 0, "x" 4, "y" 4}))))))

(deftest advanced-loco-test
  (with-solver (solver "advanced_loco")
    (let [[a b c d e] (for [l (map str "abcde")]
                        (int-var l 1 5))]
      (constrain! ($all-different? a b c d e)
                  ($= b ($+ a c))
                  ($= e ($* a ($+ d 1))))
      (doseq [{a "a" b "b" c "c" d "d" e "e"} (solutions)]
        (are [x] x
             (= b (+ a c))
             (= e (* a (+ d 1))))))))

;(deftest automaton-test
;  (let [a 