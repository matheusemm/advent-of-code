(ns patrulleros.aoc.y2020.day17
  (:require [patrulleros.aoc.util :as util]))

(def puzzle-input
  (util/read-lines "2020/day17.txt"))

(def example
  [".#."
   "..#"
   "###"])

(defn parse-initial-state [lines]
  (set (for [y (range (count lines))
             x (range (count (first lines)))
             :let [c (get-in lines [y x])]
             :when (= c \#)]
         [x y 0])))

(defn neighbours [[x y z :as cube]]
  (disj (set (for [dx [-1 0 1]
                   dy [-1 0 1]
                   dz [-1 0 1]]
               [(+ x dx) (+ y dy) (+ z dz)]))
        cube))

(defn step [cubes]
  (reduce (fn [state [cube n]]
            (cond
              (and (cubes cube) (<= 2 n 3))
              (conj state cube)

              (and (not (cubes cube)) (= n 3))
              (conj state cube)

              :else state))
          #{}
          (frequencies (mapcat neighbours cubes))))

(defn solve-p1
  ([]
   (solve-p1 puzzle-input))
  ([lines]
   (let [boot (iterate step (parse-initial-state lines))]
     (->> boot (drop 6) first count))))

(comment
  (assert (= 267 (solve-p1)) "Part 1 solution is wrong."))
