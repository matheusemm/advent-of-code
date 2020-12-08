(ns patrulleros.aoc.y2020.day7
  (:require [clojure.core.logic :as logic]
            [clojure.core.logic.pldb :as pldb]
            [clojure.string :as str]
            [patrulleros.aoc.util :as util])
  (:import (java.util.regex Pattern)))

(def puzzle-input
  (delay (util/read-lines "2020/day7.txt")))

(def example
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])

(def another-example
  ["shiny gold bags contain 2 dark red bags."
   "dark red bags contain 2 dark orange bags."
   "dark orange bags contain 2 dark yellow bags."
   "dark yellow bags contain 2 dark green bags."
   "dark green bags contain 2 dark blue bags."
   "dark blue bags contain 2 dark violet bags."
   "dark violet bags contain no other bags."])

(defn parse-rule [rule]
  (let [[container & contained]
        (->> (str/split rule #"bags contain|bags?[,\.]")
             (map str/trim))]
    (if (= contained ["no other"])
      []
      (reduce (fn [rels c]
                (let [[_ n b] (re-find #"(\d+) (\w+\s\w+)" c)]
                  (conj rels [container [b (Integer/parseInt n)]])))
              []
              contained))))

(pldb/db-rel contains container contained)

(defn bags [rules]
  (let [relations (mapcat parse-rule rules)]
    (reduce (fn [db [container contained]]
              (pldb/db-fact db contains container contained))
            (pldb/db)
            relations)))

(defn parentso [bag parents]
  (logic/fresh [contained]
    (contains parents contained)
    (logic/firsto contained bag)))

(logic/defne ancestorso [bag ancestors]
  ([bag ancestors]
   (parentso bag ancestors))
  ([bag ancestors]
   (logic/fresh [parents]
     (parentso bag parents)
     (ancestorso parents ancestors))))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([rules]
   (pldb/with-db (bags rules)
     (-> (logic/run* [q]
           (ancestorso "shiny gold" q))
         set
         count))))

(defn rules-map [rules]
  (reduce (fn [m [container [contained qty]]]
            (update m container (fnil assoc {}) contained qty))
          {}
          (mapcat parse-rule rules)))

(defn count-contained-bags [bag rules]
  (let [rm (rules-map rules)]
    (loop [[[bag factor] & others] [[bag 1]]
           cnt 0]
      (if bag
        (let [contained (rm bag)
              qty (apply + (vals contained))]
          (recur (concat others
                         (map (fn [[b q]] [b (* factor q)])
                              contained))
                 (+ cnt (* qty factor))))
        cnt))))

;; TODO: Rewrite this solution using logic programming

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([rules]
   (count-contained-bags "shiny gold" rules)))

(comment
  (assert (= 287 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 48160 (solve-p2)) "Part 2 solution is wrong."))
