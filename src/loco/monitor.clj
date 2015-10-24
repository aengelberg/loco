(ns loco.monitor
  (:require [loco.core :refer [*monitors*]])
  (:import (org.chocosolver.solver.search.loop.monitors
            IMonitorClose
            IMonitorContradiction
            IMonitorDownBranch
            IMonitorInitPropagation
            IMonitorInitialize
            IMonitorInterruption
            IMonitorOpenNode
            IMonitorRestart
            IMonitorSolution
            IMonitorUpBranch)))

(defn on-solution
  "Returns a solution monitor that triggers whenever a solution is
  found. Takes a callback that takes no arguments and does some
  side-effect. loco.core/current-solution would be useful to fetch the
  solved values during the execution of the callback."
  [callback]
  (reify
    IMonitorSolution
    (onSolution [this]
      (callback))))

(defmacro with-monitors
  "Usage: (with-monitors [(on-...) ...] (solution ...))"
  [monitors & body]
  `(binding [*monitors* (into *monitors* ~monitors)]
     ~@body))
