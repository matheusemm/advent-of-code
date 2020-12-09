(ns patrulleros.aoc.y2020.day9
  (:require [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (mapv #(Long/parseLong %)
               (util/read-lines "2020/day9.txt"))))

(def example
  [35
   20
   15
   25
   47
   40
   62
   55
   65
   95
   102
   117
   150
   182
   127
   219
   299
   277
   309
   576])

(defn pair-adds-to-target? [target numbers]
  (let [ns (set numbers)]
    (reduce (fn [res n]
              (let [compl (- target n)]
                (if (ns compl)
                  (reduced true)
                  res)))
            false
            numbers)))

(defn solve-p1
  ([] (solve-p1 @puzzle-input 25))
  ([numbers preamble-size]
   (->> numbers
        (partition (inc preamble-size) 1)
        (map reverse)
        (map #(let [[target & factors] %]
                [target (pair-adds-to-target? target factors)]))
        (filter #(not (second %)))
        (ffirst))))

(defn solve-p2
  ([] (solve-p2 @puzzle-input 25))
  ([numbers preamble-size]
   (let [invalid (solve-p1 numbers preamble-size)

         slice
         (reduce (fn [slice n]
                   (if (= n invalid)
                     []
                     (let [sum (apply + n slice)]
                       (cond
                         (= sum invalid)
                         (reduced (conj slice n))

                         (< sum invalid)
                         (conj slice n)

                         :else
                         (loop [slice (conj (subvec slice 1) n)]
                           (let [sum (apply + slice)]
                             (cond
                               (= sum invalid)
                               (reduced slice)

                               (< sum invalid)
                               slice

                               :else
                               (recur (subvec slice 1)))))))))
                 []
                 numbers)]
     (+ (apply min slice)
        (apply max slice)))))

(comment
  (assert (= 26796446 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 3353494 (solve-p2)) "Part 2 solution is wrong."))
