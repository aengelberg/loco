(ns loco.strategy
  (:import (org.chocosolver.solver Solver)
           (org.chocosolver.solver.search.strategy ISF))
  (:require [loco.core :as core :refer [->choco* *solver*]]))

(defn- grab-choco-variables
  "Takes a list of variables or :all, returns the Choco variable objects"
  [selector]
  (let [var-map (:my-vars *solver*)]
    (if (= :all selector)
      (vals var-map)
      (for [var-name selector]
        (or (var-map var-name)
            (throw (IllegalArgumentException.
                    (str "Var name " var-name
                         " used in strategy but not declared in constraint model"))))))))

(defn dom-over-w-deg
  ([] (dom-over-w-deg :all))
  ([vars] {:type :dom-over-w-deg
           :vars vars}))
(defmethod ->choco* :dom-over-w-deg
  [{:keys [vars]}]
  (ISF/domOverWDeg vars))
