(ns br.com.patrulleros.2021.day4
  (:require [br.com.patrulleros.xf :as xf]
            [clojure.string :as str]
            [br.com.patrulleros.input :as input]))

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

(defn create-board
  "Creates a bingo board, a map with keys `numbers` (the sequence of numbers) and `indexes` (a mapping between a board
  number and its index, between 0 and 24)."
  [numbers]
  (let [ns (mapv #(Integer/parseInt %) numbers)]
    {:numbers ns
     :indexes (into {} (map-indexed #(vector %2 %1) ns))}))

(defn parse-boards
  "Returns a set of parsed bingo boards read from `boards-lines`."
  [[_ & boards-lines]]
  (let [xf (comp (mapcat #(str/split % #" "))
                 (filter (comp not str/blank?))
                 (xf/sliding 25)
                 (map create-board))]
    (transduce xf conj #{} boards-lines)))

(defn mark-board
  [{:keys [indexes] :as board} n]
  (if-let [idx (indexes n)]
    (update-in board [:numbers idx] (constantly :x))
    board))

(defn won?
  [{:keys [numbers]}]
  (let [some-won? (fn [groups]
                    (some (fn [group] (every? #(= % :x) group))
                          groups))
        rows (partition 5 numbers)]
    (or (some-won? rows)
        (some-won? (apply map vector rows)))))

(defn sum-unmarked
  [{:keys [numbers]}]
  (->> numbers (filter number?) (apply +)))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day4-input.txt")))
  ([lines]
   (loop [[n & ns] (map #(Integer/parseInt %)
                        (str/split (first lines) #","))
          boards (parse-boards lines)]
     (let [updated-boards (set (map #(mark-board % n) boards))
           winner (first (filter won? updated-boards))]
       (if winner
         (* (sum-unmarked winner) n)
         (recur ns updated-boards))))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day4-input.txt")))
  ([lines]
   (loop [[n & ns] (map #(Integer/parseInt %)
                        (str/split (first lines) #","))
          boards (parse-boards lines)]
     (let [boards (set (map #(mark-board % n) boards))
           winners (filter won? boards)
           keep-playing (apply disj boards winners)]
       (if (seq keep-playing)
         (recur ns keep-playing)
         (* (sum-unmarked (first winners)) n))))))

