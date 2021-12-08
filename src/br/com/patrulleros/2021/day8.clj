(ns br.com.patrulleros.2021.day8
  (:require [br.com.patrulleros.input :as input]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example
  ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
   "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
   "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
   "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
   "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
   "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
   "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
   "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
   "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
   "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"])

(defn parse-line
  [line]
  (let [parts (str/split line #"(\s\|\s)|\s")]
    {:signals (->> (subvec parts 0 10)
                   (mapv set)
                   (group-by count))
     :output (mapv set (subvec parts 10))}))

(defn parse-lines
  [lines]
  (mapv parse-line lines))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day8-input.txt")))
  ([lines]
   (->> (parse-lines lines)
        (mapcat :output)
        (filter #(#{2 3 4 7} (count %)))
        (count))))

(defn decode-d3
  [signals col-r]
  (->> (signals 5)
       (filter #(= col-r (set/intersection % col-r)))
       (first)))

(defn decode-d0-d6-d9
  [signals col-l col-r]
  (let [sigs (set (signals 6))
        cols (set/union col-l col-r)
        ;; d0: the one that has the complete left and right "columns"
        d0 (->> sigs
                (filter #(= cols (set/intersection % cols)))
                (first))
        ;; d6: one of the other two that has the complete left "column"
        d6 (->> (disj sigs d0)
                (filter #(= col-l (set/intersection % col-l)))
                (first))
        d9 (first (disj sigs d0 d6))]
    [d0 d6 d9]))

(defn decode-d2-d5
  [signals d3 d4]
  (let [sigs (disj (set (signals 5)) d3)
        ;; d2: there are only two signals in common between d2 and d4
        d2 (->> sigs
                (filter #(= 2 (count (set/intersection % d4))))
                (first))
        d5 (first (disj sigs d2))]
    [d2 d5]))

(defn decode
  [{:keys [signals output]}]
  (let [d1 (get-in signals [2 0])
        d7 (get-in signals [3 0])
        d4 (get-in signals [4 0])
        d8 (get-in signals [7 0])

        col-r (set/intersection d1 d7)
        d3 (decode-d3 signals col-r)
        [d2 d5] (decode-d2-d5 signals d3 d4)

        col-l (set/difference d8 d3)
        [d0 d6 d9] (decode-d0-d6-d9 signals col-l col-r)

        dec-table {d0 0 d1 1 d2 2 d3 3 d4 4 d5 5 d6 6 d7 7 d8 8 d9 9}]
    (->> output
         (map dec-table)
         (apply str)
         (Integer/parseInt))))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day8-input.txt")))
  ([lines]
   (->> (parse-lines lines)
        (map decode)
        (apply +))))

