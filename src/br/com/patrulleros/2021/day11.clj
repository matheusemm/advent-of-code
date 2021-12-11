(ns br.com.patrulleros.2021.day11
  (:require [br.com.patrulleros.input :as input]
            [clojure.set :as set]
            [clojure.string :as str]))

(def example
  ["5483143223"
   "2745854711"
   "5264556173"
   "6141336146"
   "6357385478"
   "4167524645"
   "2176841721"
   "6882881134"
   "4846848554"
   "5283751526"])

(def small-example
  ["11111"
   "19991"
   "19191"
   "19991"
   "11111"])

(defn print-octos
  [octopuses]
  (let [size (int (Math/sqrt (count octopuses)))]
    (->> octopuses
         (sort-by first)
         (map #(format "%3s" (second %)))
         (partition size)
         (map #(apply str %))
         (str/join "\n")
         (println))))

(defn parse-line
  [y line]
  (->> line
       (map-indexed #(vector [y %1] (Character/digit ^char %2 10)))
       (into {})))

(defn parse-octopuses
  [lines]
  (apply merge (map-indexed parse-line lines)))

(def neighbours
  (memoize
    (fn [[y x]]
      (set (for [dx [-1 0 1]
                 dy (if (zero? dx) [-1 1] [-1 0 1])]
             [(+ y dy) (+ x dx)])))))

(defn increase-energies
  ([octopuses]
   (increase-energies octopuses (keys octopuses)))
  ([octopuses ks]
   (reduce (fn [octopuses pos]
             (if (contains? octopuses pos)
               (update octopuses pos inc)
               octopuses))
           octopuses
           ks)))

(defn full-energy?
  [[_ energy]]
  (> energy 9))

(defn step
  [octopuses]
  (loop [octopuses (increase-energies octopuses)
         [flashing & others] (->> octopuses (filter full-energy?) (map first))
         flashed #{}]
    (if flashing
      (let [affected (set/difference (neighbours flashing) flashed)
            octopuses (-> octopuses
                          (assoc flashing 0)
                          (increase-energies affected))
            more-others (->> affected
                             (map #(find octopuses %))
                             (filter #(and % (full-energy? %)))
                             (map first))]
        (recur octopuses
               (apply conj (set others) more-others)
               (conj flashed flashing)))
      {:octopuses octopuses, :flashes (count flashed)})))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day11-input.txt")))
  ([lines]
   (let [octopuses (parse-octopuses lines)
         it (iterate (fn [{:keys [octopuses flashes]}]
                       (let [res (step octopuses)]
                         (update res :flashes + flashes)))
                     {:octopuses octopuses, :flashes 0})]
     (->> it (drop 100) (first) :flashes))))

(defn all-flashed?
  [octopuses]
  (every? zero? (vals octopuses)))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day11-input.txt")))
  ([lines]
   (let [octopuses (parse-octopuses lines)
         it (iterate (fn [{:keys [octopuses n]}]
                       (let [res (step octopuses)]
                         (assoc res :n (inc n))))
                     {:octopuses octopuses, :n 0})]
     (->> it (filter #(all-flashed? (:octopuses %))) (first) :n))))

