(ns patrulleros.aoc.y2020.day6
  (:require [clojure.set :as set]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day6.txt")))

(def example
  ["abc"
   ""
   "a"
   "b"
   "c"
   ""
   "ab"
   "ac"
   ""
   "a"
   "a"
   "a"
   "a"
   ""
   "b"])

(defn split-groups [answers]
  (->> answers
       (partition-by #(= "" %))
       (filter #(not= [""] %))))

(defn count-anyone-yes [answers]
  (->> answers (mapcat seq) set count))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([answers]
   (->> answers
        (split-groups)
        (map count-anyone-yes)
        (apply +))))

(defn count-everyone-yes [answers]
  (->> answers
       (map set)
       (apply set/intersection)
       (count)))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([answers]
   (->> answers
        (split-groups)
        (map count-everyone-yes)
        (apply +))))

(comment
  (assert (= 6551 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 3358 (solve-p2)) "Part 2 solution is wrong."))
