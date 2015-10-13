(ns loco.automata
  (:refer-clojure :exclude [cat])
  (:import (org.chocosolver.solver.constraints.nary.automata.FA
            FiniteAutomaton)
           (dk.brics.automaton
            Automaton)))

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
      (.setFinal f ^int (state->int-state state)))
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
  "Takes a regular expression that parses a sequence of integers instead of characters.
  It has the following syntax:
  - Any digit (0-9) parses that number.
  - An integer within angle-brackets (<12>) parses that integer.
  (careful, \"12\" without angle brackets parses 1, then 2.)
  - Special characters like ()[]|+*? work as expected.
  - Using letters, whitespace, or other non-digit non-special characters has unsupported,
  unintuitive behavior, and it is recommended that you avoid them."
  [^String s]
  (FiniteAutomaton. s))

(defn union
  "Takes two automata A1 and A2, and returns a new automaton that
  accepts an input iff A1 or A2 would accept it."
  [^FiniteAutomaton A1 ^FiniteAutomaton A2]
  (.union A1 A2))

(defn intersection
  "Takes two automata A1 and A2, and returns a new automaton that
  accepts an input iff both A1 and A2 would accept it."
  [^FiniteAutomaton A1 ^FiniteAutomaton A2]
  (.intersection A1 A2))

(defn cat
  "Takes two automata A1 and A2, and returns a new automaton that
  accepts an input S iff there exists two strings S1 and S2, such that
  a1 accepts S1, a2 accepts S2, and S1 + S2 = S."
  [^FiniteAutomaton A1 ^FiniteAutomaton A2]
  (.concatenate A1 A2))

(defn minimize!
  "Mutates an automaton to have the minimal number of states necessary.
  Optionally specify what algorithm to use:
  - :hopcroft (O(n log n) algorithm) (default)
  - :huffman (O(n^2) algorithm)
  - :brzozowski (O(2^n) algorithm)
  If the input automaton is non-deterministic, minimize! will first
  have to \"determinize\" it (i.e. ensure only one transition per input
  character per state) which is an exponential-complexity operation."
  ([a]
   (minimize! a :hopcroft))
  ([^FiniteAutomaton a algorithm]
   (Automaton/setMinimization
    (case algorithm
      :hopcroft Automaton/MINIMIZE_HOPCROFT
      :brzozowski Automaton/MINIMIZE_BRZOZOWSKI
      :huffman Automaton/MINIMIZE_HUFFMAN))
   (.minimize a)
   a))
