(ns sudoku-solver.core
  (:require [clojure.string :as str])
  (:gen-class))

(def empty-cell nil)
(def e empty-cell)
(def empty-row (vec (repeat 9 empty-cell)))
(def empty-board (vec (repeat 9 empty-row)))
(def all-board-coords (for [row (range 1 10) col (range 1 10)] [row col]))
(def possible-cell-values (range 1 10))

(defn board->string [board]
  (str/join "\n\n"
            (map
              (fn [row]
                (str/join
                  "  "
                  (map (fn [cell]
                         (if (= empty-cell cell)
                           "."
                           cell))
                       row)))
              board)))

(defn get-cell
  [board row col]
  (get-in board [(dec row) (dec col)]))

(defn set-cell
  [board row col value]
  (assoc-in board [(dec row) (dec col)] value))

(defn get-row
  [board row]
  (get board (dec row)))

(defn get-col
  [board col]
  (vec (map (fn [row] (get-cell board row col))
            (range 1 10))))

(defn is-coord-in-grid?
  [[row col] [start-row start-col] [end-row end-col]]
  (and
    (and (>= row start-row) (<= row end-row))
    (and (>= col start-col) (<= col end-col))))

(defn get-grid
  [board start end]
  (let [coords-in-grid (filter #(is-coord-in-grid? % start end) all-board-coords)
        grid-cells (map (fn [[row col]] (get-cell board row col)) coords-in-grid)]
    (vec grid-cells)))

(defn coord->grid-edges
  [row col]
  (let [grid-edges [[[1 1] [3 3]]
                    [[1 4] [3 6]]
                    [[1 7] [3 9]]

                    [[4 1] [6 3]]
                    [[4 4] [6 6]]
                    [[4 7] [6 9]]

                    [[7 1] [9 3]]
                    [[7 4] [9 6]]
                    [[7 7] [9 9]]]]
    (first (filter (fn [[start end]]
                     (is-coord-in-grid? [row col] start end)) grid-edges))))

(defn coord->grid
  [board row col]
  (let [[grid-start grid-end] (coord->grid-edges row col)]
    (get-grid board grid-start grid-end)))

(defn find-first-empty-cell
  [board]
  (first (filter
           (fn [[row col]] (= empty-cell (get-cell board row col)))
           all-board-coords)))

(defn get-possible-values
  [board row col]
  (let [row-values (get-row board row)
        col-values (get-col board col)
        grid-values (coord->grid board row col)]
    (vec (remove (fn [possible-value]
                   (let [col-has-value? (fn [col] (some #(= possible-value %) col))]
                     (or
                       (col-has-value? row-values)
                       (col-has-value? col-values)
                       (col-has-value? grid-values))))
                 possible-cell-values))))

(defn solve
  [board]
  (let [cell-to-fill (find-first-empty-cell board)]

    (if (nil? cell-to-fill)
      [:solved, board]

      (let [[row col] cell-to-fill
            values-to-try (get-possible-values board row col)]

        (if (empty? values-to-try)
          [:failed, board]

          (loop [remaining-values-to-try values-to-try]
            (if (empty? remaining-values-to-try)
              [:failed, board]

              (let [current-value-being-tried (first remaining-values-to-try)
                    board-with-current-value (set-cell board row col current-value-being-tried)
                    [status, potentially-solved-board] (solve board-with-current-value)]

                (if (= status :solved)
                  [:solved, potentially-solved-board]

                  (recur (rest remaining-values-to-try)))))))))))

(def puzzle-1-cells
  [[1 1 5]
   [1 2 3]
   [1 5 7]

   [2 1 6]
   [2 4 1]
   [2 5 9]
   [2 6 5]

   [3 2 9]
   [3 3 8]
   [3 8 6]

   [4 1 8]
   [4 5 6]
   [4 9 3]

   [5 1 4]
   [5 4 8]
   [5 6 3]
   [5 9 1]

   [6 1 7]
   [6 5 2]
   [6 9 6]

   [7 2 6]
   [7 7 2]
   [7 8 8]

   [8 4 4]
   [8 5 1]
   [8 6 9]
   [8 9 5]

   [9 5 8]
   [9 8 7]
   [9 9 9]])

(def puzzle-1-board
  (reduce (fn [board [row col val]] (set-cell board row col val))
          empty-board
          puzzle-1-cells))

(def puzzle-2-board
  [[e 5 e 2 e e e e 3]
   [9 e e e e 4 e 5 6]
   [e e e e e 8 e 7 e]
   [e 1 7 e e e 6 e e]
   [6 e e e e e e e 8]
   [e e 9 e e e 4 2 e]
   [e 9 e 7 e e e e e]
   [5 6 e 1 e e e e 7]
   [8 e e e e 5 e 6 e]])

(def puzzle-3-board
  [[e e e e 1 4 e e 5]
   [e e e 3 e 7 e e e]
   [e 7 e 6 e e 1 e e]
   [5 e 9 e e 2 8 e 4]
   [e 2 4 e e e 7 1 e]
   [6 e 7 8 e e 5 e 9]
   [e e 2 e e 5 e 9 e]
   [e e e 9 e 1 e e e]
   [7 e e 4 8 e e e e]])

(defn -main
  [& args]
  (println "Hello, World!"))
