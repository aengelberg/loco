(ns loco.strategy
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

  Optional args:
   :seed - random seed (long) - default = 0
   :vars - list of vars to consider in this strategy - default = all vars
   :val-selector - value selector to use after selecting var - default = min value in domain"
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
