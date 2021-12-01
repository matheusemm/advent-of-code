(ns br.com.patrulleros.input
  (:require [clojure.java.io :as io]))

(defn read-lines
  [resource]
  (with-open [reader (io/reader (io/resource resource))]
    (into [] (line-seq reader))))

(defn read-ints
  [resource]
  (with-open [reader (io/reader (io/resource resource))]
    (->> reader (line-seq) (mapv #(Integer/parseInt %)))))
