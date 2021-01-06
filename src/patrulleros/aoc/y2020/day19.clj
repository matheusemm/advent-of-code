(ns patrulleros.aoc.y2020.day19
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day19.txt")))

(def example
  ["0: 4 1 5"
   "1: 2 3 | 3 2"
   "2: 4 4 | 5 5"
   "3: 4 5 | 5 4"
   "4: \"a\""
   "5: \"b\""
   ""
   "ababbb"
   "bababa"
   "abbbab"
   "aaabbb"
   "aaaabbb"])

(defn parse-rule [s]
  (let [[id & productions] (str/split s #"(:\s|\s\|\s)")
        id (Integer/parseInt id)
        productions (if (str/starts-with? (first productions) "\"")
                      (.charAt (first productions) 1)
                      (->> productions
                           (map #(str/split % #"\s"))
                           (map #(mapv (fn [c] (Integer/parseInt c)) %))
                           (set)))]
    [id productions]))

(defn parse-rules [ss]
  (into {} (map parse-rule ss)))

(defn matches-char? [[c & cs :as message] expected]
  (if (= c expected)
    [c cs]
    [nil message]))

(defn matches-rule? [message rule-id rules]
  (let [productions (rules rule-id)]
    (if (char? productions)
      (matches-char? message productions)
      (matches-productions? message productions rules))))

(defn matches-production? [message production rules]
  (reduce (fn [[matched to-match] rule-id]
            (let [res (matches-rule-id? to-match rule-id rules)]
              (if (first res)
                res
                (reduced [nil message]))))
          [nil message]
          production))

(defn matches-productions? [message productions rules]
  (reduce (fn [[matched to-match] production]
            (let [res (matches-production? to-match production rules)]
              (if (first res)
                (reduced res)
                [matched to-match])))
          [nil message]
          productions))

(defn matches? [message element rules]
  (cond
    (vector? element)
    (matches-productions? message element rules)

    (int? element)
    (matches-rule? message element rules)

    (char? element)
    (matches-char? message element)))

(defn count-matches [messages rule-id rules]
  (->> messages
       (map #(matches? % rule-id rules))
       (filter #(not (second %)))
       (count)))

(defn solve-p1
  ([]
   (solve-p1 @puzzle-input))
  ([input]
   (let [[rules [_ & messages]] (split-with #(not= "" %) input)
         rules (parse-rules rules)]
     (count-matches messages 0 rules))))

(comment
  (assert (= 205 (solve-p1)) "Part 1 solution is wrong."))
