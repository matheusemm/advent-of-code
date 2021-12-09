(ns br.com.patrulleros.2021.day9
  (:require [br.com.patrulleros.input :as input]
            [clojure.set :as set]))

(def example
  ["2199943210"
   "3987894921"
   "9856789892"
   "8767896789"
   "9899965678"])

(defn parse-line
  [line]
  (mapv #(Character/digit % 10) line))

(defn parse-heightmap
  [lines]
  (mapv parse-line lines))

(defn neighbours
  [[y x] heightmap]
  (let [nrows (count heightmap)
        ncols (count (first heightmap))]
    (set
      (filter (fn [[y x]] (and (< -1 y nrows)
                               (< -1 x ncols)))
              #{[y (dec x)] [y (inc x)] [(dec y) x] [(inc y) x]}))))


(defn low-point?
  [location heightmap]
  (let [height (get-in heightmap location)]
    (every? #(< height %)
            (->> (neighbours location heightmap)
                 (map #(get-in heightmap %))
                 (set)))))

(defn low-points
  [heightmap]
  (let [nrows (count heightmap)
        ncols (count (first heightmap))]
    (for [x (range ncols)
          y (range nrows)
          :let [location [y x]]
          :when (low-point? location heightmap)]
      location)))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day9-input.txt")))
  ([lines]
   (let [heightmap (parse-heightmap lines)
         heights (map #(get-in heightmap %) (low-points heightmap))]
     (+ (count heights) (apply + heights)))))

(defn find-basin
  [location heightmap]
  (loop [[loc & locs] [location]
         basin #{loc}]
    (if loc
      (let [height (get-in heightmap loc)
            ns (->> basin
                    (set/difference (neighbours loc heightmap))
                    (filter #(let [h (get-in heightmap %)]
                               (and (not= h 9) (> h height)))))]
        (recur (apply conj locs ns)
               (apply conj basin ns)))
      basin)))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day9-input.txt")))
  ([lines]
   (let [heightmap (parse-heightmap lines)
         points (low-points heightmap)
         basins (map #(find-basin % heightmap) points)]
     (->> basins
          (map count)
          (sort >)
          (take 3)
          (apply *)))))
