(ns patrulleros.aoc.y2020.day5
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day5.txt")))

(def examples
  ["BFFFBBFRRR"
   "FFFBBBFRRR"
   "BBFFBBFRLL"])

(defn decode-row [seat]
  (-> (subs seat 0 7)
      (str/replace "F" "0")
      (str/replace "B" "1")
      (Integer/parseInt 2)))

(defn decode-col [seat]
  (-> (subs seat 7 10)
      (str/replace "R" "1")
      (str/replace "L" "0")
      (Integer/parseInt 2)))

(defn decode-seat-id [seat]
  (let [row (decode-row seat)
        col (decode-col seat)]
    (+ col (* row 8))))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([boarding-passes]
   (->> boarding-passes
        (map decode-seat-id)
        (apply max))))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([boarding-passes]
   (let [ids (->> boarding-passes
                  (map decode-seat-id)
                  (sort)
                  (partition 2 1)
                  (drop-while #(= (second %) (inc (first %)))))]
     (inc (ffirst ids)))))

(comment
  (assert (= 904 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 669 (solve-p2)) "Part 2 solution is wrong."))
