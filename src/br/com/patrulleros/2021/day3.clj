(ns br.com.patrulleros.2021.day3
  (:require [br.com.patrulleros.input :as input]))

(def example ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day3-input.txt")))
  ([report-lines]
   (->> (apply map vector report-lines)
        (map (fn [col-bits]
               (let [groups (group-by identity col-bits)
                     g-bit (if (> (count (groups \0)) (count (groups \1))) \0 \1)
                     e-bit (if (= g-bit \0) \1 \0)]
                 {:gamma g-bit :epsilon e-bit})))
        (apply merge-with str)
        (map #(Integer/parseInt (second %) 2))
        (apply *))))

(defn filter-bit
  "Returns the bit (0 or 1) that will be used to filter (report) `lines` based on their values on index `idx`. `comp`
  is a comparison function used to select the resulting bit (`>` for oxygen generator rating and `<=` for C02 scrubber
  rating)."
  [comp idx lines]
  (let [counts (reduce #(update %1 (nth %2 idx) inc) {\0 0 \1 0} lines)]
    (if (comp (counts \0) (counts \1)) \0 \1)))

(defn rating
  [comp report-lines]
  (loop [lines report-lines
         i 0]
    (let [fb (filter-bit comp i lines)
          lines (filter #(= (nth % i) fb) lines)]
      (if (= (count lines) 1)
        (Integer/parseInt (first lines) 2)
        (recur lines (inc i))))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day3-input.txt")))
  ([report-lines]
   (* (rating > report-lines)
      (rating <= report-lines))))

