(ns ^{:author "Andrea Richiardi"
      :doc "Magic Series example + tests. It makes use of loco's $cardinality global constraint.
            A series S = (S0,...Sn) is magic if Si represents the number of occurrences of i in S.
            For example a Magic Series of length 5 is |2|1|2|0|0|"}
  loco.integer.magicseries
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

(defn frequency-map
  "Builds the frequency map of the series in input for the $cardinality
  constraint."
  [series]
  (into {} (for [[k i] series] [i [k i]])))

(defn ms-model
  "Return the model."
  [series-length]
  (let [series (series-vars series-length)] 
    (conj (concat series (numeric-constraints series)) 
          ($cardinality series (frequency-map series)))))

(defn reorder-solution
  "Manages to rearrange a solution in a better way."
  [solution]
  (into (sorted-map) solution))

(deftest magicseries-tests
  "Testing Magic Series solution #1. One solution given a length."

  ;; No solution for length < 4
  (is (empty? (solution (ms-model 1))) "Length 1 does not have solution")
  (is (empty? (solution (ms-model 2))) "Length 2 does not have solution")
  (is (empty? (solution (ms-model 3))) "Length 3 does not have solution")

  ;; No solution for length = 6 (try it out!)
  (is (empty? (solution (ms-model 6))) "Length 6 does not have solution")
  
  ;; Testing between 4 (inclusive) and 50 (inclusive)
  (doseq [l (take 10 (repeatedly #(+ 4 (rand-int 47))))]
    (let [sol (solution (ms-model l))] 
      #_(println "Solution for length:" l " " (reorder-solution sol)) 

      (doseq [[[k i] occ] (seq sol)]
        (is (= occ (count (filter #(= i %1) (map second sol)))))))))
