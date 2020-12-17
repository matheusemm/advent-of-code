(ns patrulleros.aoc.y2020.day14
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day14.txt")))

(def example
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
   "mem[8] = 11"
   "mem[7] = 101"
   "mem[8] = 0"])

(def example-p2
  ["mask = 000000000000000000000000000000X1001X"
   "mem[42] = 100"
   "mask = 00000000000000000000000000000000X0XX"
   "mem[26] = 1"])

(defn init-state []
  {:mask nil
   :memory {}})

(defn pad-left [s]
  (let [n (- 36 (count s))]
    (str (str/join (repeat n \0)) s)))

(defn execute-mask [state instruction]
  (assoc state :mask (subs instruction 7)))

(defn execute-mem [state instruction]
  (let [[_ saddr svalue] (re-find #"mem\[(\d+)\] = (\d+)" instruction)
        addr (Long/parseLong saddr)
        value (-> (str/join
                   (map #(if (= %1 \X) %2 %1)
                        (:mask state)
                        (-> svalue Long/parseLong Long/toBinaryString pad-left)))
                  (Long/parseLong 2))]
    (update state :memory assoc addr value)))

(defn execute-instruction [state instruction]
  (if (str/starts-with? instruction "mask")
    (execute-mask state instruction)
    (execute-mem state instruction)))

(defn execute-program [instructions]
  (reduce execute-instruction (init-state) instructions))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([instructions]
   (let [final-state (execute-program instructions)]
     (apply + (vals (:memory final-state))))))

(defn generate-addresses [addrs-mask]
  (loop [[bitmask & bits] addrs-mask
         addrs [[]]]
    (cond
      (#{\0 \1} bitmask)
      (recur bits (map #(conj % bitmask) addrs))

      (= \X bitmask)
      (recur bits (mapcat #(vector (conj % \0) (conj % \1)) addrs))

      :else
      (map #(-> % str/join (Long/parseLong 2)) addrs))))

(defn execute-mem-v2 [state instruction]
  (let [[_ saddr svalue] (re-find #"mem\[(\d+)\] = (\d+)" instruction)
        value (Long/parseLong svalue)
        addrs-mask (str/join
                    (map #(cond
                            (= %1 \0) %2
                            (= %1 \1) \1
                            :else \X)
                         (:mask state)
                         (-> saddr Long/parseLong Long/toBinaryString pad-left)))]
    (reduce #(update %1 :memory assoc %2 value)
            state
            (generate-addresses addrs-mask))))

(defn execute-instruction-v2 [state instruction]
  (if (str/starts-with? instruction "mask")
    (execute-mask state instruction)
    (execute-mem-v2 state instruction)))

(defn execute-program-v2 [instructions]
  (reduce execute-instruction-v2 (init-state) instructions))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([instructions]
   (let [final-state (execute-program-v2 instructions)]
     (apply + (vals (:memory final-state))))))

(comment
  (assert (= 9967721333886 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 4355897790573 (solve-p2)) "Part 2 solution is wrong."))
