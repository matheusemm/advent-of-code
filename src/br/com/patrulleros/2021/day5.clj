(ns br.com.patrulleros.2021.day5
  (:require [br.com.patrulleros.input :as input]))

(def example
  ["0,9 -> 5,9"
   "8,0 -> 0,8"
   "9,4 -> 3,4"
   "2,2 -> 2,1"
   "7,0 -> 7,4"
   "6,4 -> 2,0"
   "0,9 -> 2,9"
   "3,4 -> 1,4"
   "0,0 -> 8,8"
   "5,5 -> 8,2"])

(def line-regex #"^(\d+),(\d+) -> (\d+),(\d+)$")

(defn vertical?
  [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn horizontal?
  [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn parse-line
  [line-spec]
  (let [[_ & points] (re-find line-regex line-spec)
        [x1 y1 x2 y2] (map #(Integer/parseInt %) points)]
    [[x1 y1] [x2 y2]]))

(defn line-points
  [[[x1 y1] [x2 y2] :as line]]
  (cond
    (vertical? line)
    (let [start (min y1 y2) end (inc (max y1 y2))]
      (for [y (range start end)] [x1 y]))

    (horizontal? line)
    (let [start (min x1 x2) end (inc (max x1 x2))]
      (for [x (range start end)] [x y1]))

    :else
    (let [next-x (if (> x1 x2) dec inc)
          next-y (if (> y1 y2) dec inc)
          n (inc (Math/abs ^long (- x1 x2)))]
      (take n (iterate (fn [[x y]] [(next-x x) (next-y y)])
                       [x1 y1])))))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day5-input.txt")))
  ([lines-specs]
   (let [xf (comp (map parse-line)
                  (filter #(or (vertical? %1) (horizontal? %1)))
                  (mapcat line-points))
         points (transduce xf (completing #(update %1 %2 (fnil inc 0))) {} lines-specs)]
     (count (filter #(> (second %) 1) points)))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day5-input.txt")))
  ([lines-specs]
   (let [xf (comp (map parse-line)
                  (mapcat line-points))
         points (transduce xf (completing #(update %1 %2 (fnil inc 0))) {} lines-specs)]
     (count (filter #(> (second %) 1) points)))))
