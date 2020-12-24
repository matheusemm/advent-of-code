(ns patrulleros.aoc.y2020.day17
  (:require [patrulleros.aoc.util :as util]))

(def puzzle-input
  (util/read-lines "2020/day17.txt"))

(def example
  [".#."
   "..#"
   "###"])

(defn parse-initial-state-p1 [lines]
  (set (for [y (range (count lines))
             x (range (count (first lines)))
             :let [c (get-in lines [y x])]
             :when (= c \#)]
         [x y 0])))

(defn neighbours-p1 [[x y z :as cube]]
  (disj (set (for [dx [-1 0 1]
                   dy [-1 0 1]
                   dz [-1 0 1]]
               [(+ x dx) (+ y dy) (+ z dz)]))
        cube))

(defn step-p1 [cubes]
  (reduce (fn [state [cube n]]
            (cond
              (and (cubes cube) (<= 2 n 3))
              (conj state cube)

              (and (not (cubes cube)) (= n 3))
              (conj state cube)

              :else state))
          #{}
          (frequencies (mapcat neighbours-p1 cubes))))

(defn solve-p1
  ([]
   (solve-p1 puzzle-input))
  ([lines]
   (let [boot (iterate step-p2 (parse-initial-state-p1 lines))]
     (->> boot (drop 6) first count))))

(defn parse-initial-state-p2 [lines]
  (set (for [y (range (count lines))
             x (range (count (first lines)))
             :let [c (get-in lines [y x])]
             :when (= c \#)]
         [x y 0 0])))

(defn neighbours-p2 [[x y z w :as hypercube]]
  (disj (set (for [dx [-1 0 1]
                   dy [-1 0 1]
                   dz [-1 0 1]
                   dw [-1 0 1]]
               [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))
        hypercube))

(defn step-p2 [hypercubes]
  (reduce (fn [state [hypercube n]]
            (cond
              (and (hypercubes hypercube) (<= 2 n 3))
              (conj state hypercube)

              (and (not (hypercubes hypercube)) (= n 3))
              (conj state hypercube)

              :else state))
          #{}
          (frequencies (mapcat neighbours-p2 hypercubes))))

(defn solve-p2
  ([]
   (solve-p2 puzzle-input))
  ([lines]
   (let [boot (iterate step-p2 (parse-initial-state-p2 lines))]
     (->> boot (drop 6) first count))))

(comment
  (assert (= 267 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 1812 (solve-p2)) "Part 2 solution is wrong."))
