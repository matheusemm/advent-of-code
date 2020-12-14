(ns patrulleros.aoc.y2020.day11
  (:require [clojure.set :as set]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day11.txt")))

(def example
  ["L.LL.LL.LL"
   "LLLLLLL.LL"
   "L.L.L..L.."
   "LLLL.LL.LL"
   "L.LL.LL.LL"
   "L.LLLLL.LL"
   "..L.L....."
   "LLLLLLLLLL"
   "L.LLLLLL.L"
   "L.LLLLL.LL"])

(defn step1 [grid]
  (let [groups (->> (for [y (range (count grid))
                          x (range (count (first grid)))]
                      [(get-in grid [y x]) [x y]])
                    (group-by first))]
    {:occupied (set (map second (groups \L)))
     :floor (set (map second (groups \.)))
     :free #{}}))

(defn step [{:keys [occupied free] :as occupations} count-occupied occupied-limit]
  (reduce (fn [occs seat]
            (let [n (count-occupied occupations seat)]
              (cond
                (and (occupied seat) (> n occupied-limit))
                (-> occs
                    (update :occupied disj seat)
                    (update :free conj seat))

                (and (free seat) (zero? n))
                (-> occs
                    (update :occupied conj seat)
                    (update :free disj seat))

                :else occs)))
          occupations
          (set/union occupied free)))

(defn neighbours [seats floor]
  (let [seat-neighbours (fn [[x y]]
                          (set (for [dx [-1 0 1]
                                     dy (if (zero? dx) [-1 1] [-1 0 1])
                                     :let [sn [(+ x dx) (+ y dy)]]
                                     :when (not (floor sn))]
                                 sn)))]
    (reduce #(assoc %1 %2 (seat-neighbours %2))
            {}
            seats)))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([seats-layout]
   (let [grid (mapv vec seats-layout)
         s1 (step1 grid)
         ns (neighbours (:occupied s1) (:floor s1))
         count-occupied #(->> (ns %2) (filter (:occupied %1)) count)
         occupations (iterate #(step % count-occupied 3) s1)]
     (->> occupations
          (partition 2 1)
          (drop-while #(not= (first %) (second %)))
          ffirst :occupied count))))

(defn count-occupied-p2 [{:keys [occupied floor]} seat]
  (let [deltas [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]]]
    (reduce (fn [cnt [dx dy]]
              (let [s (->> (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) seat)
                           (drop 1)
                           (drop-while #(floor %))
                           first)]
                (if (occupied s)
                  (inc cnt)
                  cnt)))
            0
            deltas)))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([seats-layout]
   (let [grid (mapv vec seats-layout)
         occupations (iterate #(step % count-occupied-p2 4) (step1 grid))]
     (->> occupations
          (partition 2 1)
          (drop-while #(not= (first %) (second %)))
          ffirst :occupied count))))

(comment
  (assert (= 2406 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 2149 (solve-p2)) "Part 2 solution is wrong."))
