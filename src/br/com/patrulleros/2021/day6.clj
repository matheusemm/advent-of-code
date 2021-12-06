(ns br.com.patrulleros.2021.day6
  (:require [br.com.patrulleros.input :as input]
            [clojure.string :as str]))

(defn parse
  [line]
  (->> (str/split line #",")
       (map #(Integer/parseInt %))
       (group-by identity)
       (map #(vector (first %) (count (second %))))
       (into {})))

(defn simulate-day
  [fish-school]
  (reduce (fn [school [timer qty]]
            (let [timer (dec timer)]
              (if (neg? timer)
                (-> school
                    (update 6 (fnil + 0) qty)
                    (update 8 (fnil + 0) qty))
                (update school timer (fnil + 0) qty))))
          {}
          fish-school))

(defn simulate
  [fish-school n-days]
  (loop [school fish-school
         day 0]
    (if (= day n-days)
      school
      (recur (simulate-day school) (inc day)))))

(defn part-1
  ([]
   (part-1 (first (input/read-lines "2021/day6-input.txt"))))
  ([line]
   (loop [school (simulate (parse line) 80)]
     (apply + (vals school)))))

(defn part-2
  ([]
   (part-2 (first (input/read-lines "2021/day6-input.txt"))))
  ([line]
   (loop [school (simulate (parse line) 256)]
     (apply + (vals school)))))
