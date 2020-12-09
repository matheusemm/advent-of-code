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

(defn set-instruction-executed
  ([program]
   (set-instruction-executed program true))
  ([{:keys [iptr] :as program} flag]
   (assoc-in program [:instructions iptr 2] flag)))

(defn set-operation [{:keys [iptr] :as program} op]
  (-> program
      (set-instruction-executed false)
      (assoc-in [:instructions iptr 0] op)))

(defn set-status [program status]
  (assoc program :status status))

(defn update-instruction-pointer
  ([program]
   (update-instruction-pointer program 1))
  ([program n]
   (update program :iptr + n)))

(defn update-accumulator [program n]
  (update program :acc + n))

(defn status [program]
  (:status program))

(defn loop-detected? [program]
  (= (:status program) :loop-detected))

(defn finished? [program]
  (= (:status program) :finished))

(defn accumulator [program]
  (:acc program))

(defn current-instruction [{:keys [instructions iptr] :as program}]
  (nth instructions iptr))

(defn execute-instruction [{:keys [instructions iptr] :as program}]
  (if (>= iptr (count instructions))
    (set-status program :finished)
    (let [[op arg executed?] (current-instruction program)]
      (cond
        executed?
        (set-status program :loop-detected)

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
            (update-instruction-pointer arg))))))

(defn execute-program [program stop-pred]
  (->> program
       (iterate execute-instruction)
       (drop-while #(not (stop-pred %)))
       (first)))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([instructions]
   (-> instructions
       prepare-program
       (execute-program loop-detected?)
       accumulator)))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([instructions]
   (let [states (->> instructions
                     (prepare-program)
                     (iterate execute-instruction)
                     (take-while #(not (loop-detected? %)))
                     (reverse)
                     (rest))]
     (loop [[s & ss] states]
       (let [op (-> s current-instruction first)]
         (if (#{:jmp :nop} op)
           (let [alternative (set-operation s (if (= op :jmp) :nop :jmp))
                 res (execute-program alternative status)

                 ]
             (if (finished? res)
               (accumulator res)
               (recur ss)))
           (recur ss)))))))

(comment
  (assert (= 1262 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 1643 (solve-p2)) "Part 2 solution is wrong."))
