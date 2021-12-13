(ns br.com.patrulleros.2021.day12
  (:require [br.com.patrulleros.input :as input]
            [clojure.set :as set]
            [clojure.string :as str]))

(def example-1
  ["start-A"
   "start-b"
   "A-c"
   "A-b"
   "b-d"
   "A-end"
   "b-end"])

(def example-2
  ["dc-end"
   "HN-start"
   "start-kj"
   "dc-start"
   "dc-HN"
   "LN-dc"
   "HN-end"
   "kj-sa"
   "kj-HN"
   "kj-dc"])

(def example-3
  ["fs-end"
   "he-DX"
   "fs-he"
   "start-DX"
   "pj-DX"
   "end-zg"
   "zg-sl"
   "zg-pj"
   "pj-he"
   "RW-he"
   "fs-DX"
   "pj-RW"
   "zg-RW"
   "start-pj"
   "he-WI"
   "zg-he"
   "pj-fs"
   "start-RW"])

(defn big-cave?
  [cave]
  (every? #(Character/isUpperCase ^char %) cave))

(defn parse-cave-system
  [lines]
  (->> lines
       (map #(str/split % #"-"))
       (reduce (fn [system [cave-1 cave-2]]
                 (-> system
                     (update cave-1 (fnil conj #{}) cave-2)
                     (update cave-2 (fnil conj #{}) cave-1)))
               {})))

(defn generate-next-path
  [{:keys [caves small-caves repeat?]} next-cave]
  (cond
    (big-cave? next-cave)
    {:caves (conj caves next-cave)
     :small-caves small-caves
     :repeat? repeat?}

    (not (small-caves next-cave))
    {:caves (conj caves next-cave)
     :small-caves (conj small-caves next-cave)
     :repeat? repeat?}

    (and (small-caves next-cave) repeat?)
    {:caves (conj caves next-cave)
     :small-caves (conj small-caves next-cave)
     :repeat? false}))

(defn generate-paths
  ([cave-system]
   (generate-paths cave-system false))
  ([cave-system repeat-small-cave?]
   (let [initial-path {:caves ["start"], :small-caves #{} :repeat? repeat-small-cave?}]
     (loop [[path & ps] #{initial-path}
            paths #{}]
       (if path
         (let [{:keys [caves]} path
               previous-cave (peek caves)]
           (if (= previous-cave "end")
             (recur ps (conj paths caves))
             (let [next-caves (disj (cave-system previous-cave) "start")]
               (if (seq next-caves)
                 (let [next-paths (->> next-caves
                                       (map (partial generate-next-path path))
                                       (remove nil?))]
                   (recur (apply conj ps next-paths) paths))
                 (recur ps paths)))))
         paths)))))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day12-input.txt")))
  ([lines]
   (-> lines parse-cave-system generate-paths count)))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day12-input.txt")))
  ([lines]
   (-> lines parse-cave-system (generate-paths true) count)))
