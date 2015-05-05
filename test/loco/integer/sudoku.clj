(ns loco.integer.sudoku
  "The obligatory sudoku example"
  (:use loco.core
        loco.constraints
        clojure.test))

(def sudoku-base-model
  "Base board constraints, without the puzzle-specific hints."
  (concat
   ;; every cell is between 1-9
   (for [i (range 9)
         j (range 9)]
     ($in [:cell i j] 1 9))
   ;; each row contains 1-9 exactly once
   (for [i (range 9)]
     ($distinct (for [j (range 9)] [:cell i j])))
   ;; each column contains 1-9 exactly once
   (for [j (range 9)]
     ($distinct (for [i (range 9)] [:cell i j])))
   ;; each 3x3 section contains 1-9 exactly once
   (for [x (range 3)
         y (range 3)]
     ($distinct (for [i (range (* x 3) (* (inc x) 3))
                      j (range (* y 3) (* (inc y) 3))]
                  [:cell i j])))))

(defn sudoku-puzzle->model
  "Takes a starting board, a vector of vectors.
If numbers are found in cells, they will be given as \"hints\"
to further define the specific puzzle."
  [starting-board]
  (let [hints (for [i (range 9)
                    j (range 9)
                    :when (integer? (get-in starting-board [i j]))]
                ($= [:cell i j] (get-in starting-board [i j])))]
    (concat sudoku-base-model hints)))

(defn solution->board
  "Given a solution map, prettifies it into a vector of vectors"
  [sol]
  (mapv vec
        (for [i (range 9)]
          (for [j (range 9)]
            (sol [:cell i j])))))

(def sample-puzzle
  '[[- - -   - - -   - - -]
    [- - -   - - 3   - 8 5]
    [- - 1   - 2 -   - - -]

    [- - -   5 - 7   - - -]
    [- - 4   - - -   1 - -]
    [- 9 -   - - -   - - -]

    [5 - -   - - -   - 7 3]
    [- - 2   - 1 -   - - -]
    [- - -   - 4 -   - - 9]])

(def sample-solution
  [[9 8 7   6 5 4   3 2 1]
   [2 4 6   1 7 3   9 8 5]
   [3 5 1   9 2 8   7 4 6]

   [1 2 8   5 3 7   6 9 4]
   [6 3 4   8 9 2   1 5 7]
   [7 9 5   4 6 1   8 3 2]

   [5 1 9   2 8 6   4 7 3]
   [4 7 2   3 1 9   5 6 8]
   [8 6 3   7 4 5   2 1 9]])

(deftest sudoku-test
  (is (= (->> sample-puzzle
           sudoku-puzzle->model
           solutions
           (map solution->board))
         (list sample-solution))))
