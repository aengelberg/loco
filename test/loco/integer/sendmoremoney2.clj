(ns ^{:author "Andrea Richiardi"
      :doc "Send + More = Money example + tests, second version." }
  loco.integer.sendmoremoney2
  (:refer-clojure :exclude [send]) 
  (:require [clojure.test :refer :all]
            [loco.core :refer :all]
            [loco.constraints :refer :all]
            [criterium.core :refer [bench]]))

(defn costraint-0-9 [vars]
  (for [v vars]
    ($in v 0 9)))

(def letters
  [:S :E :N :D :M :O :R :Y])

(def carries
  (for [i (range 4)]
    ($in [:_c i] 0 1)))

(def ^{:doc "Naive model, $distinct on each letter. Constraint on the sum with carries:
               c3 c2 c1 c0     
                S  E  N  D +
                M  O  R  E =
             ---------------
             M  O  N  E  Y  "}
  smm-naive-model
  (concat (costraint-0-9 letters)
          carries
          [($distinct letters)
           ($!= :S 0)
           ($!= :M 0)
           ($= ($+         :D :E) ($+ ($* 10 [:_c 0]) :Y))
           ($= ($+ [:_c 0] :N :R) ($+ ($* 10 [:_c 1]) :E))
           ($= ($+ [:_c 1] :E :O) ($+ ($* 10 [:_c 2]) :N))
           ($= ($+ [:_c 2] :S :M) ($+ ($* 10 [:_c 3]) :O))
           ($=     [:_c 3]                            :M)]))

(defn reorder-solution
  "Manages to rearrange a solution in a better way."
  [solution]
  (hash-map :send [(find solution :S) (find solution :E) (find solution :N) (find solution :D)]
            :more [(find solution :M) (find solution :O) (find solution :R) (find solution :E)]
            :money [(find solution :M) (find solution :O) (find solution :N) (find solution :E) (find solution :Y)]))

(defn random-solution
  "Returns a random-picked solution, some gobbledygook was necessary to
  rearrange it."
  []
  (if-let [sols (solutions smm-naive-model)] (rand-nth sols) ()))

(def bench-solution
  #(bench (solution smm-naive-model)))

(def bench-solutions
  #(bench (solutions smm-naive-model)))

(deftest sendmoremoney-naive2
  "Testing Send + More = Money solution #2"

  (doseq [sol (solutions smm-naive-model)]
    (let [num-of #(reduce (fn [acc [i [_ v]]] (+ acc (* v (long (Math/pow 10 i)))))
                          0
                          (map-indexed vector (reverse %1)))
          letter-of (fn [map letter] (filter #(= (first %1) letter) map))
          reordered-sol (reorder-solution sol)]
      #_(println "Solution:" sol)
      #_(println "Reordered:" reordered-sol)
      #_(println "In numbers:" (num-of (:send reordered-sol)) "+" (num-of (:more reordered-sol)) "=" (num-of (:money reordered-sol)))

      (is (= (num-of (:money reordered-sol)) (+ (num-of (:send reordered-sol)) (num-of (:more reordered-sol)))) "They sum up just right.")
      (is (not= 0 (:S sol)) "First letter not zero 1")
      (is (not= 0 (:M sol)) "First letter not zero 2")
      (is (= (letter-of (:send reordered-sol) \e) (letter-of (:more reordered-sol) \e)) "Letter e 1 matches")
      (is (= (letter-of (:send sol) \e) (letter-of (:money sol) \e)) "Letter e 2 matches")
      (is (= (letter-of (:more sol) \e) (letter-of (:money sol) \e)) "Letter e 3 matches")
      (is (= (letter-of (:send sol) \n) (letter-of (:money sol) \n)) "Letter n matches")
      (is (= (letter-of (:more sol) \o) (letter-of (:money sol) \o)) "Letter o matches")
      (is (= (letter-of (:more sol) \m) (letter-of (:money sol) \m)) "Letter m matches"))))

