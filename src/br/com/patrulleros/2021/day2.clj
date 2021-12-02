(ns br.com.patrulleros.2021.day2
  (:require [br.com.patrulleros.input :as input]
            [clojure.string :as str]))

(defn parse-instruction
  [instruction]
  (let [[direction amount] (str/split instruction #" ")]
    [direction (Integer/parseInt amount)]))

(defn update-position
  [position [direction amount]]
  (condp = direction
    "down" (update position :depth + amount)
    "up" (update position :depth - amount)
    "forward" (update position :horizontal + amount)))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day2-input.txt")))
  ([instructions]
   (let [{:keys [horizontal depth]}
         (->> instructions
              (map parse-instruction)
              (reduce update-position {:horizontal 0 :depth 0}))]
     (* horizontal depth))))

(defn update-state
  [state [direction amount]]
  (condp = direction
    "down" (update state :aim + amount)
    "up" (update state :aim - amount)
    "forward" (let [depth (* amount (:aim state))]
                (-> state
                    (update :horizontal + amount)
                    (update :depth + depth)))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day2-input.txt")))
  ([instructions]
   (let [{:keys [horizontal depth]}
         (->> instructions
              (map parse-instruction)
              (reduce update-state {:horizontal 0 :depth 0 :aim 0}))]
     (* horizontal depth))))
