(ns patrulleros.aoc.y2020.day16
  (:require [clojure.set :as set]
            [clojure.string :as str]
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

(def example-p2
  ["class: 0-1 or 4-19"
   "row: 0-5 or 8-19"
   "seat: 0-13 or 16-19"
   ""
   "your ticket:"
   "11,12,13"
   ""
   "nearby tickets:"
   "3,9,18"
   "15,1,5"
   "5,14,9"])

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

(defn valid-ticket? [values validators]
  (every? (fn [value]
            (some #(% value) validators))
          values))

(defn get-valid-tickets [{:keys [fields nearby-tickets]}]
  (let [validators (map :validator fields)]
    (filter #(valid-ticket? % validators) nearby-tickets)))

(defn get-fields-values [tickets]
  (apply mapv vector tickets))

(defn get-validators [{:keys [fields]}]
  (set (map :validator fields)))

(defn sfirst [xs]
  (second (first xs)))

(defn decode-fields-order [document]
  (let [fields-values (-> document get-valid-tickets get-fields-values)
        fields-cols (->> (for [field (:fields document)
                               idx (range (count fields-values))
                               :when (every? (:validator field) (nth fields-values idx))]
                           [(:name field) idx])
                         (group-by first)
                         (map (fn [[name values]]
                                [name (set (map second values))]))
                         (sort-by (comp count second) >=))]
    (reduce (fn [fields [[nname ncols] [name cols]]]
              (let [col (first (set/difference ncols cols))]
                (assoc fields col nname)))
            (sorted-map)
            (partition 2 1 fields-cols))))

(defn solve-p2
  ([]
   (solve-p2 @puzzle-input))
  ([sdoc]
   (let [document (parse-document sdoc)
         departure-fields-indexes (->> (decode-fields-order document)
                                       (filter #(str/starts-with? (second %) "departure"))
                                       (map first))]
     (->> departure-fields-indexes
          (select-keys (:ticket document))
          (vals)
          (apply *)))))

(comment
  (assert (= 25895 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 5865723727753 (solve-p2)) "Part 2 solution is wrong."))
