(ns patrulleros.aoc.y2020.day3
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day3.txt")))

(def example
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(defn step [cols [dx dy] [x y]]
  [(mod (+ x dx) cols) (+ y dy)])

(defn steps [cols deltas start-xy]
  (iterate (partial step cols deltas) start-xy))

(defn index [cols [x y]]
  (+ x (* y cols)))

(defn tree-indexes-xf [grid]
  (let [cols (count (first grid))
        terrain (str/join grid)]
    (comp
     (map (partial index cols))
     (take-while #(< % (count terrain)))
     (map #(nth terrain %))
     (filter #(= \# %)))))

(defn count-trees [cols delta grid]
  (->> (steps cols delta [0 0])
       (sequence (tree-indexes-xf grid))
       (count)))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([grid]
   (let [cols (count (first grid))]
     (count-trees cols [3 1] grid))))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([grid]
   (let [cols (count (first grid))
         trees (for [delta [[1 1] [3 1] [5 1] [7 1] [1 2]]]
                 (count-trees cols delta grid))]
     (apply * trees))))

(comment
  (assert (= 294 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 5774564250 (solve-p2)) "Part 2 solution is wrong."))
