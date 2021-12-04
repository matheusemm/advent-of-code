(ns br.com.patrulleros.2021.day4
  (:require [br.com.patrulleros.input :as input]
            [br.com.patrulleros.xf :as xf]
            [clojure.string :as str]))

(def example
  ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
   ""
   "22 13 17 11  0"
   "8  2 23  4 24"
   "21  9 14 16  7"
   "6 10  3 18  5"
   "1 12 20 15 19"
   ""
   "3 15  0  2 22"
   "9 18 13 17  5"
   "19  8  7 25 23"
   "20 11 10 24  4"
   "14 21 16 12  6"
   ""
   "14 21 17 24  4"
   "10 16 15  9 19"
   "18  8 23 26 20"
   "22 11 13  6  5"
   "2  0 12  3  7"])

(def mark :X)
(def n-rows 5)
(def n-cols 5)

(defn parse-drawn-numbers
  [[s]]
  (mapv #(Integer/parseInt %) (str/split s #",")))

(defn create-board
  [ns]
  (let [numbers (mapv #(Integer/parseInt %) ns)]
    {:numbers numbers
     :indexes (into {} (map-indexed #(vector %2 %1) numbers))}))

(defn parse-boards
  [[_ & boards-lines]]
  (let [xf (comp (mapcat #(str/split % #"\s+"))
                 (filter (comp not str/blank?))
                 (xf/sliding (* n-rows n-cols))
                 (map create-board))]
    (transduce xf conj #{} boards-lines)))

(defn mark-board
  [{:keys [indexes] :as board} n]
  (if-let [idx (indexes n)]
    (update-in board [:numbers idx] (constantly mark))
    board))

(defn won?
  [{:keys [numbers]}]
  (let [marked? #(= % mark)
        complete? (fn [rows-or-cols] (some #(every? marked? %) rows-or-cols))
        rows (partition n-cols numbers)]
    (or (complete? rows)
        (complete? (apply map vector rows)))))

(defn sum-unmarked
  [{:keys [numbers]}]
  (->> numbers (filter number?) (apply +)))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day4-input.txt")))
  ([lines]
   (let [drawn-numbers (parse-drawn-numbers lines)
         boards (parse-boards lines)]
     (reduce (fn [boards n]
               (let [marked (map #(mark-board % n) boards)
                     winner (first (filter won? marked))]
                 (if winner
                   (reduced (* (sum-unmarked winner) n))
                   (set marked))))
             boards
             drawn-numbers))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day4-input.txt")))
  ([lines]
   (let [drawn-numbers (parse-drawn-numbers lines)
         boards (parse-boards lines)]
     (reduce (fn [boards n]
               (let [marked (set (map #(mark-board % n) boards))
                     winners (filter won? marked)
                     non-winners (apply disj marked winners)]
                 (if (seq non-winners)
                   non-winners
                   (reduced (* (sum-unmarked (first winners)) n)))))
             boards
             drawn-numbers))))
