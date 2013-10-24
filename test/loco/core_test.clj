(ns loco.core-test
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basic-loco
  (with-solver (solver "basic_loco")
    (let [x (int-var "x" 1 5)
          y (int-var "y" 5 10)
          z (int-var "z" 10 15)]
      (constrain! ($= ($+ x 3) y)
                  ($= ($+ y 2) z))
      (is (= (solutions) '({:solution 0, "z" 10, "y" 8, "x" 5}))))))

(deftest basic-loco2