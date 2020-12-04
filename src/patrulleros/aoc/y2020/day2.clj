(ns patrulleros.aoc.y2020.day2
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day2.txt")))

(def example
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

(def line-parser #"^(\d+)\-(\d+) (\w): (\w+)$")

(defn valid-sled-password? [line]
  (let [[_ lower upper [letter] password] (re-find line-parser line)]
    (<= (Integer/parseInt lower)
        (count (filter #(= letter %) password))
        (Integer/parseInt upper))))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([lines]
   (count (filter valid-sled-password? lines))))

(defn valid-toboggan-password? [line]
  (let [[_ p1 p2 [letter] password] (re-find line-parser line)
        p1 (dec (Integer/parseInt p1))
        p2 (dec (Integer/parseInt p2))]
    (if (= letter (nth password p1))
      (not= letter (nth password p2))
      (= letter (nth password p2)))))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([lines]
   (count (filter valid-toboggan-password? lines))))

(comment
  (assert (= 500 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 313 (solve-p2)) "Part 2 solution is wrong."))
