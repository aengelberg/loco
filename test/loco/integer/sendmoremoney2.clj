(ns ^{:author "Andrea Richiardi"
      :doc "Send + More = Money example solution #2 + tests. This solution takes the long road of
            having a var for each letter, constraints on the addends of the sum plus carries and
            does not make use of loco's $scalar constraint (see sendmoremoney1.clj for a more
            concise implementation)." }
  loco.integer.sendmoremoney2
  (:refer-clojure :exclude [send]) 
  (:require [clojure.test :refer :all]
            [loco.core :refer :all]
            [loco.constraints :refer :all]))

(defn initialize-digits [vars]
  (for [v vars]
    ($in v 0 9)))

(def letters
  [:S :E :N :D :M :O :R :Y])

(def carries
  (for [i (range 4)]
    ($in [:_c i] 0 1)))

(def smm2-model
  "Model #2. Constraints on the addends with carries:
               c3 c2 c1 c0     
                S  E  N  D +
                M  O  R  E =
             ---------------
             M  O  N  E  Y  "
  (concat (initialize-digits letters)
          carries
          [($distinct letters)
           ($!= :S 0)
           ($!= :M 0)
           ($= ($+         :D :E) ($+ ($* 10 [:_c 0]) :Y))
           ($= ($+ [:_c 0] :N :R) ($+ ($* 10 [:_c 1]) :E))
           ($= ($+ [:_c 1] :E :O) ($+ ($* 10 [:_c 2]) :N))
           ($= ($+ [:_c 2] :S :M) ($+ ($* 10 [:_c 3]) :O))
           ($=     [:_c 3]                            :M)]))

(deftest sendmoremoney-tests
  "Testing Send + More = Money solution #2"

  (is (= 1 (count (solutions smm2-model))) "Just one solution")
  (is (= (solutions smm2-model) '({:S 9 :E 5 :N 6 :D 7 :M 1 :O 0 :R 8 :Y 2})) "Has correct solution"))

