(ns patrulleros.aoc.y2020.day16
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day16.txt")))

(def example
  ["class: 1-3 or 5-7"
   "row: 6-11 or 33-44"
   "seat: 13-40 or 45-50"
   ""
   "your ticket:"
   "7,1,14"
   ""
   "nearby tickets:"
   "7,3,47"
   "40,4,50"
   "55,2,20"
   "38,6,12"])

(defn parse-ticket-values [ticket-line]
  (mapv #(Integer/parseInt %) (str/split ticket-line #",")))

(defn parse-field [field-line]
  (let [[_ name & ranges-values] (re-find #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" field-line)
        ranges (->> ranges-values
                    (map #(Integer/parseInt %))
                    (partition 2)
                    (mapv vec))]
    {:name name
     :ranges ranges
     :validator (fn [value]
                  (some (fn [[start end]]
                          (<= start value end))
                        ranges))}))

(defn parse-document [sdoc]
  (let [[fields _ ticket _ other-tickets] (partition-by #(not= % "") sdoc)]
    {:fields (mapv parse-field fields)
     :ticket (parse-ticket-values (second ticket))
     :nearby-tickets (mapv parse-ticket-values (rest other-tickets))}))

(defn get-never-valid-field-values [document]
  (for [value (flatten (:nearby-tickets document))
        :let [validators (map :validator (:fields document))]
        :when (every? #(not (% value)) validators)]
    value))

(defn calculate-ticket-scanning-error-rate [document]
  (apply + (get-never-valid-field-values document)))

(defn solve-p1
  ([]
   (solve-p1 @puzzle-input))
  ([sdoc]
   (-> sdoc parse-document calculate-ticket-scanning-error-rate)))

(comment
  (assert (= 25895 (solve-p1)) "Part 1 solution is wrong."))
