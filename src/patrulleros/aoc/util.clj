(ns patrulleros.aoc.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-lines [resource]
  (-> resource io/resource slurp str/split-lines))

(defn read-integers [resource]
  (mapv #(Integer/parseInt %) (read-lines resource)))
