(ns patrulleros.aoc.y2020.day18
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (util/read-lines "2020/day18.txt"))

(def examples
  ["1 + 2 * 3 + 4 * 5 + 6"
   "2 * 3 + (4 * 5)"
   "5 + (8 * 3 + 9 + 3 * 4 * 3)"
   "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
   "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"])

(defn digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn scan-token [[c & src :as source]]
  (if (digit? c)
    (let [[digits src] (split-with digit? source)
          n (Integer/parseInt (str/join digits))]
      [n src])
    (case c
      \+ [+ src]
      \* [* src]
      [c src])))

(defn scan
  ([source]
   (scan-tokens [] (seq (str/replace source " " ""))))
  ([tokens source]
   (if (seq source)
     (let [[token src] (scan-token source)]
       (recur (conj tokens token) src))
     tokens)))

(defn parse [parser tokens]
  (first (parser tokens)))

(defn parse-expr-p1 [tokens]
  (let [[expr tokens] (parse-primary parse-expr-p1 tokens)]
    (loop [left expr
           [op? & rtokens] tokens]
      (if (#{+ *} op?)
        (let [[right tkns] (parse-primary parse-expr-p1 rtokens)]
          (recur [:BINARY op? left right] tkns))
        [left (conj rtokens op?)]))))

(defn parse-primary [root-parser [token & tokens]]
  (if (= token \()
    (let [[expr tokens] (root-parser tokens)]
      [[:GROUPING expr] (rest tokens)])
    [token tokens]))

(defn evaluate [expr]
  (if (sequential? expr)
    (if (= (first expr) :GROUPING)
      (evaluate (second expr))
      (let [[_ operator left right] expr]
        (operator (evaluate left) (evaluate right))))
    expr))

(defn solve-p1
  ([]
   (solve-p1 puzzle-input))
  ([exprs-source]
   (->> exprs-source
        (map (comp evaluate (partial parse parse-expr-p1) scan))
        (apply +))))

(defn parse-binary [op parser tokens]
  (let [[expr tokens] (parser tokens)]
    (loop [left expr
           [op? & rtokens] tokens]
      (if (= op? op)
        (let [[right tkns] (parser rtokens)]
          (recur [:BINARY op? left right] tkns))
        [left (conj rtokens op?)]))))

(declare parse-expr-p2)

(defn parse-addition [tokens]
  (parse-binary + (partial parse-primary parse-expr-p2) tokens))

(defn parse-multiplication [tokens]
  (parse-binary * parse-addition tokens))

(defn parse-expr-p2 [tokens]
  (parse-multiplication tokens))

(defn solve-p2
  ([]
   (solve-p2 puzzle-input))
  ([exprs-source]
   (->> exprs-source
        (map (comp evaluate (partial parse parse-expr-p2) scan))
        (apply +))))

(comment
  (assert (= 280014646144 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 9966990988262 (solve-p2)) "Part 2 solution is wrong."))
