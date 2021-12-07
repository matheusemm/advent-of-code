(ns br.com.patrulleros.2021.day7
  (:require [br.com.patrulleros.input :as input]
            [clojure.string :as str]))

(defn parse-positions
  [positions-line]
  (->> (str/split positions-line #",")
       (map #(Integer/parseInt %))
       (frequencies)
       (into (sorted-map))))

(defn solve
  [positions-line cost-fn]
  (let [positions (parse-positions positions-line)
        [min-p] (first positions)
        [max-p] (last positions)]
    (reduce (fn [res pos]
              (min res (reduce #(+ %1 (cost-fn pos %2))
                               0
                               positions)))
            Long/MAX_VALUE
            (range min-p (inc max-p)))))

(defn cheap-cost
  [ref [pos n]]
  (* n (Math/abs ^long (- ref pos))))

(defn part-1
  ([]
   (part-1 (first (input/read-lines "2021/day7-input.txt"))))
  ([positions-line]
   (solve positions-line cheap-cost)))

(defn expensive-cost
  [ref [pos n]]
  (let [distance (Math/abs ^long (- ref pos))
        ;; Sum of the terms of an arithmetic progression between 1 and `distance` with step 1.
        cost (/ (* distance (inc distance)) 2)]
    (* n cost)))

(defn part-2
  ([]
   (part-2 (first (input/read-lines "2021/day7-input.txt"))))
  ([positions-line]
   (solve positions-line expensive-cost)))
