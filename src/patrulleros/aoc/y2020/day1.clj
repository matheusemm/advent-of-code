(ns patrulleros.aoc.y2020.day1
  (:require [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-integers "2020/day1.txt")))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([expenses]
   (reduce (fn [cache expense]
             (if-let [compl (cache expense)]
               (reduced (* compl expense))
               (let [compl (- 2020 expense)]
                 (assoc cache compl expense))))
           {}
           expenses)))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([expenses]
   (reduce (fn [cache expense]
             (let [[x y] (cache expense)]
               (if (and x y)
                 (reduced (* expense x y))
                 (let [cache (->> cache
                                  (filter #(< expense (first %)))
                                  (map (fn [[k v]] [(- k expense) (conj v expense)]))
                                  (into cache))]
                   (assoc cache (- 2020 expense) [expense])))))
           {}
           expenses)))

(comment
  (assert (= 842016 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 9199664 (solve-p2)) "Part 2 solution is wrong."))
