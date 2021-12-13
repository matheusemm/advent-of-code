(ns br.com.patrulleros.2021.day13
  (:require [br.com.patrulleros.input :as input]
            [clojure.string :as str]))

(def example
  ["6,10"
   "0,14"
   "9,10"
   "0,3"
   "10,4"
   "4,11"
   "6,0"
   "6,12"
   "4,1"
   "0,13"
   "10,12"
   "3,4"
   "3,0"
   "8,4"
   "1,10"
   "2,14"
   "8,10"
   "9,0"
   ""
   "fold along y=7"
   "fold along x=5"])

(defn parse-point
  [point-line]
  (mapv #(Integer/parseInt %) (str/split point-line #",")))

(defn parse-points
  [point-lines]
  (set (map parse-point point-lines)))

(declare fold-up fold-left)

(defn parse-fold
  [fold-line]
  (let [[_ axis value] (re-find #"fold along (x|y)=(\d+)" fold-line)
        value (Integer/parseInt value)]
    (if (= axis "x")
      (partial fold-left value)
      (partial fold-up value))))

(defn parse-folds
  [fold-lines]
  (mapv parse-fold fold-lines))

(defn max-y
  [points]
  (reduce #(max %1 (second %2)) 0 points))

(defn max-x
  [points]
  (reduce #(max %1 (first %2)) 0 points))

(defn fold-up
  [y points]
  (let [maxy (max-y points)
        top (set (filter #(< (second %) y) points))
        bot (->> points
                 (filter #(> (second %) y))
                 (map (fn [[x y]] [x (- maxy y)])))]
    (apply conj top bot)))

(defn fold-left
  [x points]
  (let [maxx (max-x points)
        left (set (filter #(< (first %) x) points))
        right (->> points
                   (filter #(> (first %) x))
                   (map (fn [[x y]] [(- maxx x) y])))]
    (apply conj left right)))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day13-input.txt")))
  ([lines]
   (let [[point-lines [_ & fold-lines]] (split-with #(not (str/blank? %)) lines)
         points (parse-points point-lines)
         fold (parse-fold (first fold-lines))]
     (count (fold points)))))

(defn paper
  [points]
  (str/join \newline
            (map (fn [y]
                   (apply str
                          (map (fn [x] (if (points [x y]) \# \space))
                               (range (inc (max-x points))))))
                 (range (inc (max-y points))))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day13-input.txt")))
  ([lines]
   (let [[point-lines [_ & fold-lines]] (split-with #(not (str/blank? %)) lines)
         folded (reduce #(%2 %1)
                        (parse-points point-lines)
                        (parse-folds fold-lines))]
     (paper folded))))
