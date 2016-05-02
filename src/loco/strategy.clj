(ns loco.strategy
  (:refer-clojure :exclude [remove])
  (:import (org.chocosolver.solver Solver)
           (org.chocosolver.solver.variables IntVar)
           (org.chocosolver.solver.search.strategy ISF))
  (:require [loco.core :as core :refer [->choco* *solver*]]))

(defn- grab-choco-variables
  "Takes a list of variables or :all, returns the Choco variable objects in a IntVar array"
  [selector]
  (let [var-map @(:my-vars *solver*)]
    (into-array
     IntVar
     (if (= :all selector)
       (vals var-map)
       (for [var-name selector]
         (or (var-map var-name)
             (throw (IllegalArgumentException.
                     (str "Var name " var-name
                          " used in strategy but not declared in constraint model")))))))))

(defn dom-over-w-deg
  "Assignment strategy which selects a variable according to
  DomOverWDeg[1] and assign it to the selected value.

  Optional keyword arguments:
   :seed - random seed (long) - default = 0
   :vars - list of vars to consider in this strategy - default = all vars
   :val-selector - value selector to use after selecting var - default = min-value-selector"
  ([& {:keys [seed vars] :as opts}]
   {:type :dom-over-w-deg
    :opts opts}))
(defmethod ->choco* :dom-over-w-deg
  [{{:keys [seed vars val-selector]} :opts}]
  (let [var-objects (grab-choco-variables (or vars :all))
        seed (long (or seed 0))]
    (if val-selector
      (ISF/domOverWDeg var-objects seed val-selector)
      (ISF/domOverWDeg var-objects seed))))

(defn custom
  "Builds your own search strategy based on binary decisions.

  Required keyword arguments:
   :var-selector - How to select the variable
   :value-selector - How to select the value out of the chosen var's domain

  Optional keyword arguments:
   :vars - list of vars to consider in this strategy - default = all vars
   :decision-operator - what to do with the chosen value - default = assign"
  ([& {:keys [var-selector value-selector decision-operator vars]
       :as opts}]
   {:pre [(and var-selector value-selector)]}
   {:type :custom
    :opts opts}))
(declare assign)
(defmethod ->choco* :custom
  [{{:keys [var-selector value-selector decision-operator vars]} :opts}]
  (let [var-objects (grab-choco-variables (or vars :all))
        decision-operator (or decision-operator assign)]
    (ISF/custom var-selector value-selector decision-operator var-objects)))

;; Var selectors

(defn lexico-var-selector [] (ISF/lexico_var_selector))

(defn max-domain-size-var-selector [] (ISF/maxDomainSize_var_selector))

(defn min-domain-size-var-selector [] (ISF/minDomainSize_var_selector))

(defn max-regret-var-selector [] (ISF/maxRegret_var_selector))

(defn random-var-selector
  ([]
   (random-var-selector 0))
  ([seed]
   (ISF/random_var_selector (long seed))))

;; Value selectors

(defn max-value-selector [] (ISF/max_value_selector))

(defn min-value-selector [] (ISF/min_value_selector))

(defn mid-value-selector
  ([] (mid-value-selector true))
  ([floor?] (ISF/mid_value_selector (boolean floor?))))

(defn random-value-selector
  ([] (random-value-selector 0))
  ([seed] (ISF/random_value_selector (long seed))))

(defn random-bound-value-selector
  ([] (random-bound-value-selector 0))
  ([seed] (ISF/randomBound_value_selector (long seed))))

;; Decision operators

(def assign (ISF/assign))
(def remove (ISF/remove))
(def split (ISF/split))
(def reverse-split (ISF/reverse-split))
