(ns loco.automata
  (:import (org.chocosolver.solver.constraints.nary.automata.FA
            FiniteAutomaton)))

(defn check-state
  [all-states state]
  (when-not (contains? all-states state)
    (throw (IllegalArgumentException.
            (str "State " (pr-str state) " is not declared in the list of all states"))))
  true)

(defn make-automaton
  "Creates a Choco FiniteAutomaton object to be used in loco constraints.
  Takes as arguments:
  - A list of all the states
  - A list of all the transitions, in the form of vectors of [src dest input]
  - The initial state
  - A list of the final / accepting states
  Note that in Loco, all inputs to the state machine must be integers."
  [all-states transitions initial-state final-states]
  (prn all-states transitions initial-state final-states)
  (let [f (FiniteAutomaton.)
        all-states (set all-states)
        state->int-state (zipmap all-states (repeatedly #(.addState f)))]
    (doseq [[src dest input] transitions]
      (check-state all-states src)
      (check-state all-states dest)
      (.addTransition f (state->int-state src)
                        (state->int-state dest)
                        (int-array [input])))
    (check-state all-states initial-state)
    (.setInitialState f (state->int-state initial-state))
    (doseq [state final-states]
      (check-state all-states state)
      (.setFinal f (state->int-state state)))
    f))

(defn map->automaton
  "A more idiomatic way to create automaton objects.
  Takes a map of
  {:state {<input> :new-state
           ...}
   ...},
  as well as the initial state and final states."
  [m initial-state final-states]
  (let [all-states (set (concat (keys m)
                                (mapcat vals (vals m))))
        transitions (for [[src transition-m] m
                          [input dest] transition-m]
                      [src dest input])]
    (make-automaton all-states transitions initial-state final-states)))

(defn string->automaton
  "Takes a regular expression")
