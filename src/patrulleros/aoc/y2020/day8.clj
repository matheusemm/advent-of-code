(ns patrulleros.aoc.y2020.day8
  (:require [clojure.string :as str]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day8.txt")))

(def example
  ["nop +0"
   "acc +1"
   "jmp +4"
   "acc +3"
   "jmp -3"
   "acc -99"
   "acc +1"
   "jmp -4"
   "acc +6"])

(defn parse-instruction [instruction]
  (let [[op arg] (str/split instruction #" ")]
    [(keyword op) (Integer/parseInt arg)]))

(defn prepare-program [instructions]
  {:instructions (->> instructions
                      (map parse-instruction)
                      (mapv #(conj % false)))
   :iptr 0
   :acc 0})

(defn set-instruction-executed [{:keys [iptr] :as program}]
  (assoc-in program [:instructions iptr 2] true))

(defn set-status [program status]
  (assoc program :status status))

(defn update-instruction-pointer
  ([program]
   (update-instruction-pointer program 1))
  ([program n]
   (update program :iptr + n)))

(defn update-accumulator [program n]
  (update program :acc + n))

(defn loop-detected? [program]
  (= (:status program) :loop-detected))

(defn finished? [program]
  (= (:status program) :finished))

(defn execute-instruction [{:keys [instructions iptr acc] :as program}]
  (let [[op arg executed?] (nth instructions iptr)]
    (cond
      executed?
      (set-status program :loop-detected)

      (>= iptr (count instructions))
      (set-status program :finished)

      (= op :nop)
      (-> program
          set-instruction-executed
          update-instruction-pointer)

      (= op :acc)
      (-> program
          (set-instruction-executed)
          (update-accumulator arg)
          (update-instruction-pointer))

      (= op :jmp)
      (-> program
          (set-instruction-executed)
          (update-instruction-pointer arg)))))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([instructions]
   (let [program (prepare-program instructions)]
     (->> program
          (iterate execute-instruction)
          (drop-while #(not (loop-detected? %)))
          (first)
          :acc))))
