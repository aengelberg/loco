(ns ^{:author "Andrea Richiardi"
      :doc "Magic Series example + tests.
            A series S = (S0,...Sn) is magic if Si represents the number of occurrences of i in S.
            Example: A Magic Series of lenght 4 is |2|1|2|0|0|"}
  loco.integer.magicseries
  (:require [clojure.test :refer :all]
            [loco.core :refer :all]
            [loco.constraints :refer :all]
            [criterium.core :refer [bench]]))

(defn series-vars
  "Returns the control variables of the Magic Series."
  [series-lenght]
  (for [i (range series-lenght)]
    [:s i]))

(defn numeric-constraints
  "Builds the numeric constraints of each Magic Series control variable
  in input."
  [series]
  (let [series-lenght (count series)]
    (for [s series] 
      ($in s 0 (dec series-lenght)))))

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

(defn ms-model
  "Return the model."
  [series-lenght]
  (let [series (series-vars series-lenght)] 
    (concat series
            (numeric-constraints series)
            (occurrence-constraints series))))

(defn reorder-solution
  "Manages to rearrange a solution in a better way."
  [solution]
  (into (sorted-map) solution))

(defn random-solution
  "Returns a random-picked solution, some gobbledygook was necessary to
  rearrange it."
  [series-lenght]
  (if-let [sols (seq (solutions (ms-model series-lenght)))]
    (rand-nth sols)
    ()))

(def bench-solution
  #(bench (solution ms-model)))

(def bench-solutions
  #(bench (solutions ms-model)))

(deftest magicseries
  "Testing Magic Series solution"

  ;; One solution per model given a length, testing some random length
  (doseq [l (take 10 (repeatedly #(rand-int 50)))]
    (let [sol (solution (ms-model l))] 
      (println "Solution for length:" l " " (reorder-solution sol)) 
        
      (doseq [[[k i] occ] (seq sol)]
        (is (= occ (count (filter #(= i %1) (map second sol)))))))))
