(ns patrulleros.aoc.y2020.day10
  (:require [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (map #(Integer/parseInt %)
              (util/read-lines "2020/day10.txt"))))

(def example
  [16 10 15 5 1 11 7 19 6 12 4])

(def larger-example
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([adapter-ratings]
   (let [ratings (-> adapter-ratings (conj 0) sort vec)
         ratings (conj ratings (+ 3 (peek ratings)))]
     (->> ratings
          (partition 2 1)
          (map #(- (second %) (first %)))
          (group-by identity)
          (map #(count (second %)))
          (apply *)))))

(defn select-next-ratings [r ratings]
  (->> ratings
       (drop-while #(>= r %))
       (take-while #(<= % (+ r 3)))))

(defn update-counters [counters keys delta]
  (reduce #(update %1 %2 + delta) counters keys))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([adapter-ratings]
   (let [device-rating (+ 3 (apply max adapter-ratings))
         ratings (-> adapter-ratings
                     (conj 0 device-rating)
                     (sort))
         paths-counters (->> ratings
                             (map #(vector % 0))
                             (into {}))
         paths-counters (assoc paths-counters 0 1)
         paths-counters (reduce (fn [counters r]
                                  (let [next-ratings (select-next-ratings r ratings)]
                                    (update-counters counters next-ratings (counters r))))
                                paths-counters
                                ratings)]
     (paths-counters device-rating))))

(comment
  (assert (= 2574 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 2644613988352 (solve-p2)) "Part 2 solution is wrong."))
