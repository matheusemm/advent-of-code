(ns patrulleros.aoc.y2020.day15)

(def puzzle-input [1 12 0 20 8 16])

(def examples
  [[[0 3 6] 436]
   [[1 3 2] 1]
   [[2 1 3] 10]
   [[1 2 3] 27]
   [[2 3 1] 78]
   [[3 2 1] 438]
   [[3 1 2] 1836]])

(defn play-turn [[turn previous-n seen]]
  (let [[t1 t0] (seen previous-n)
        next-n (if t0 (- t1 t0) 0)]
    [(inc turn) next-n (update seen next-n #(if (nil? %) [turn] [turn (first %)]))]))

(defn solve [xs nth]
  (let [turn (inc (count xs))
        previous-n (peek xs)
        seen (into {} (map #(vector %1 [%2]) xs (iterate inc 1)))]
    (->> (iterate play-turn [turn previous-n seen])
         (drop-while #(<= (first %) nth))
         first
         second)))

(defn solve-p1
  ([]
   (solve-p1 puzzle-input))
  ([xs]
   (solve xs 2020)))

;; Is there any way to make the calculation faster for the 30,000,000th element?

(defn solve-p2
  ([]
   (solve-p2 puzzle-input))
  ([xs]
   (solve xs 30000000)))

(comment
  (assert (= 273 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 47205 (solve-p2)) "Part 2 solution is wrong."))
