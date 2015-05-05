(ns ^{:author "Andrea Richiardi"
      :doc "Magic Series example #2 + tests. This implementation does not make use of loco's
            $cardinality, see Magic Series #1 for that.
            A series S = (S0,...Sn) is magic if Si represents the number of occurrences of i in S.
            Example: A Magic Series of length 5 is |2|1|2|0|0|"}
  loco.integer.magicseries2
  (:require [clojure.test :refer :all]
            [loco.core :refer :all]
            [loco.constraints :refer :all]))

(defn series-vars
  "Returns the control variables of the Magic Series."
  [series-length]
  (for [i (range series-length)]
    [:s i]))

(defn numeric-constraints
  "Builds the numeric constraints of each Magic Series control variable
  in input."
  [series]
  (let [series-length (count series)]
    (for [s series] 
      ($in s 0 (dec series-length)))))

(defn occurrence-count
  "Given a k element in the Magic Series, returns a var containing the
  number of its occurrences in the series itself."
  [i series]
  (apply $+ (for [s series]
              ($reify ($= i s)))))

(defn occurrence-constraints
  "Builds the occurrence constraint of each Magic Series control variable
  in input."
  [series]
  (for [s series]
    ($= s (occurrence-count (second s) series))))

(defn ms2-model
  [series-length]
  (let [series (series-vars series-length)] 
    (concat (numeric-constraints series)
            (occurrence-constraints series))))

(deftest magicseries2-tests
  "Testing Magic Series solution #2."
  
  ;; No solution for length < 4
  (is (empty? (solutions (ms2-model 1))) "Length 1 does not have solution")
  (is (empty? (solutions (ms2-model 2))) "Length 2 does not have solution")
  (is (empty? (solutions (ms2-model 3))) "Length 3 does not have solution")

  ;; No solution for length = 6 (try it out!)
  (is (empty? (solutions (ms2-model 6))) "Length 6 does not have solution")
  
  ;; Testing between 4 (inclusive) and 50 (inclusive)
  (doseq [l (range 4 21)]
    (doseq [sol (solutions (ms2-model l))] 
      (doseq [[[k i] occ] (seq sol)]
        (is (= occ (count (filter #(= i %1) (map second sol)))))))))

