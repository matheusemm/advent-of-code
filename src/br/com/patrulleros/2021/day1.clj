(ns br.com.patrulleros.2021.day1
  (:require [br.com.patrulleros.input :as input]))

(defn count-increases
  [measurements]
  (->> measurements
       (partition 2 1)
       (filter (fn [[m1 m2]] (> m2 m1)))
       (count)))

(defn part-1
  ([]
   (part-1 (input/read-ints "2021/day1-input.txt")))
  ([measurements]
   (count-increases measurements)))

(defn part-2
  ([]
   (part-2 (input/read-ints "2021/day1-input.txt")))
  ([measurements]
   (->> measurements
        (partition 3 1)
        (map #(apply + %))
        (count-increases))))

