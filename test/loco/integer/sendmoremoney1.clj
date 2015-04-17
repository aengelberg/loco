(ns ^{:author "Andrea Richiardi"
      :doc "Send + More = Money example + tests." }
  loco.integer.sendmoremoney1
  (:refer-clojure :exclude [send]) 
  (:require [clojure.test :refer :all]
            [loco.core :refer :all]
            [loco.constraints :refer :all]
            [criterium.core :refer [bench]]))


(defn costraint-0-9 [vars]
  (for [v vars]
    ($in v 0 9))) 

(def send
  (for [l "send"]
    [:send l])) 

(def more
  (for [l "more"]
    [:more l])) 

(def money
  (for [l "money"]
    [:money l])) 

(def letter-ties
  [ ;; letter e
   ($= [:send \e] [:more \e]) 
   ($= [:send \e] [:money \e]) 
   ($= [:more \e] [:money \e]) 
   ;; letter o
   ($= [:more \o] [:money \o]) 
   ;; Letter m
   ($= [:more \m] [:money \m])
   ;; Letter n
   ($= [:send \n] [:money \n])]) 

(def carries
  (for [i (range 4)]
    ($in [:_c i] 0 1))) 

(def ^{:doc "Naive model. No $distinct on each letter but on each word, which yields multiple
 solutions. Constraint on the sum with carries:
               c3 c2 c1 c0     
                S  E  N  D +
                M  O  R  E =
             ---------------
             M  O  N  E  Y  "}
  smm-naive-model
  (concat (costraint-0-9 send)
          (costraint-0-9 more)
          (costraint-0-9 money)
          letter-ties
          carries
          [($distinct send)
           ($distinct more)
           ($distinct money)
           ($!= [:send \s] 0)
           ($!= [:more \m] 0)
           ($!= [:money \m] 0)
           
           ($= ($+         [:send \d] [:more \e]) ($+ ($* 10 [:_c 0]) [:money \y]))
           ($= ($+ [:_c 0] [:send \n] [:more \r]) ($+ ($* 10 [:_c 1]) [:money \e]))
           ($= ($+ [:_c 1] [:send \e] [:more \o]) ($+ ($* 10 [:_c 2]) [:money \n]))
           ($= ($+ [:_c 2] [:send \s] [:more \m]) ($+ ($* 10 [:_c 3]) [:money \o]))
           ($=     [:_c 3]                                            [:money \m])]))

(defn reorder-solution
  "Manages to rearrange a solution in a better way."
  [solution]
  (let [apply-hm (partial apply hash-map)
        merging-f #(merge %1 (apply-hm %2))
        sol (rand-nth (solutions smm-naive-model))
        word-maps (reduce merge
                          (for [part (partition-by ffirst (sort-by ffirst sol))]
                            (let [part-map (map apply-hm (map #(let [key (first (first %1))
                                                                     letter (second (first %1))
                                                                     value (second %1)]
                                                                 (vector key [letter value])) part))
                                  merged-val-map (first (apply merge-with concat part-map))] 

                              (hash-map (key merged-val-map) (apply-hm (val merged-val-map))))))]
    (let [s-map (:send word-maps)
          m1-map (:more word-maps)
          m2-map (:money word-maps)]
      (hash-map :send (map #(find s-map %1) "send") 
                :more (map #(find m1-map %1) "more")
                :money (map #(find m2-map %1) "money")))))

(defn random-solution
  "Returns a random-picked solution, some gobbledygook was necessary to
  rearrange it."
  []
  (if-let [sols (solutions smm-naive-model)] (rand-nth sols) ()))

(def bench-solution
  #(bench (solution smm-naive-model)))

(def bench-solutions
  #(bench (solutions smm-naive-model)))

(deftest sendmoremoney-naive1
  "Testing Send + More = Money solution #1"

  (doseq [sol (solutions smm-naive-model)]
    (let [num-of #(reduce (fn [acc [i [_ v]]] (+ acc (* v (long (Math/pow 10 i)))))
                          0
                          (map-indexed vector (reverse %1)))
          letter-of (fn [map letter] (filter #(= (first %1) letter) map))
          reordered-sol (reorder-solution sol)]
      #_(println "Solution:" reordered-sol)
      #_(println "In numbers:" (num-of (:send reordered-sol)) "+" (num-of (:more reordered-sol)) "=" (num-of (:money reordered-sol)))

      (is (= (num-of (:money reordered-sol)) (+ (num-of (:send reordered-sol)) (num-of (:more reordered-sol)))) "They sum up just right.")
      (is (not= 0 (first (:send reordered-sol))) "First letter not zero 1")
      (is (not= 0 (first (:more reordered-sol))) "First letter not zero 2")
      (is (not= 0 (first (:money reordered-sol))) "First letter not zero 3")
      (is (= (letter-of (:send reordered-sol) \e) (letter-of (:more reordered-sol) \e)) "Letter e 1 matches")
      (is (= (letter-of (:send sol) \e) (letter-of (:money sol) \e)) "Letter e 2 matches")
      (is (= (letter-of (:more sol) \e) (letter-of (:money sol) \e)) "Letter e 3 matches")
      (is (= (letter-of (:send sol) \n) (letter-of (:money sol) \n)) "Letter n matches")
      (is (= (letter-of (:more sol) \o) (letter-of (:money sol) \o)) "Letter o matches")
      (is (= (letter-of (:more sol) \m) (letter-of (:money sol) \m)) "Letter m matches"))))
