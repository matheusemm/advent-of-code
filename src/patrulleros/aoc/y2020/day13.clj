(ns patrulleros.aoc.y2020.day13
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (util/read-lines "2020/day13.txt"))

(def example
  ["939"
   "7,13,x,x,59,x,31,19"])

(defn parse-input-p1 [input]
  (let [bus-ids (->> (str/split (second input) #",")
                     (filter #(not= % "x"))
                     (mapv #(Integer/parseInt %)))]
    [(Integer/parseInt (first input)) bus-ids]))

(defn solve-p1
  ([] (solve-p1 puzzle-input))
  ([input]
   (let [[earliest bus-ids] (parse-input-p1 input)
         selected (->> bus-ids
                       (map #(vector % (int (- (* % (Math/ceil (/ earliest %)))
                                               earliest))))
                       (sort-by second)
                       (first))]
     (apply * selected))))

(defn parse-input-p2 [input]
  (->> (str/split (second input) #",")
       (map-indexed #(vector %1 %2))
       (remove #(= (second %) "x"))
       (mapv #(vector (first %)
                      (Integer/parseInt (second %))))))

;; https://www.reddit.com/r/Clojure/comments/kd6r5d/advent_of_code_2020_day_13/

(defn magic [[sum product] [index id]]
  (loop [sum sum]
    (if (zero? (mod (+ sum index) id))
      [sum (* product id)]
      (recur (+ sum product)))))

(defn solve-p2
  ([] (solve-p2 puzzle-input))
  ([input]
   (first (reduce magic (parse-input-p2 input)))))

(comment
  (assert (= 2947 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 526090562196173 (solve-p2)) "Part 2 solution is wrong."))
