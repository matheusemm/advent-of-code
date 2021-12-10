(ns br.com.patrulleros.2021.day10
  (:require [br.com.patrulleros.input :as input]))

(def example
  ["[({(<(())[]>[[{[]{<()<>>"
   "[(()[<>])]({[<{<<[]>>("
   "{([(<{}[<>[]}>{[]{[(<()>"
   "(((({<>}<{<{<>}{[]{[]{}"
   "[[<[([]))<([[{}[[()]]]"
   "[{[{({}]{}}([{[{{{}}([]"
   "{<[[]]>}<{[{[{[]{()[[[]"
   "[<(<(<(<{}))><([]([]()"
   "<{([([[(<>()){}]>(<<{{"
   "<{([{{}}[<[[[<>{}]]]>[]]"])

(def points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def open-close-delimiters {\( \), \[ \], \{ \}, \< \>})

(def close-open-delimiters
  (->> open-close-delimiters
       (map (juxt second first))
       (into {})))

(def open-delimiters
  (set (keys open-close-delimiters)))

(defn check-syntax
  [line]
  (let [res (reduce (fn [stack c]
                      (if (open-delimiters c)
                        (conj stack c)
                        (let [expected (close-open-delimiters c)
                              actual (peek stack)]
                          (if (= expected actual)
                            (pop stack)
                            (reduced {:status :corrupted :actual c :expected (open-close-delimiters actual)})))))
                    []
                    line)]
    (cond
      (map? res) res
      (seq res) {:status :incomplete :state res}
      :else {:status :valid})))

(defn part-1
  ([]
   (part-1 (input/read-lines "2021/day10-input.txt")))
  ([lines]
   (->> lines
        (map check-syntax)
        (filter #(= (:status %) :corrupted))
        (map #(points (:actual %)))
        (apply +))))

(def completion-points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn complete
  [state]
  (mapv open-close-delimiters (reverse state)))

(defn score
  [complete-state]
  (reduce #(+ (* %1 5) (completion-points %2))
          0
          complete-state))

(defn part-2
  ([]
   (part-2 (input/read-lines "2021/day10-input.txt")))
  ([lines]
   (let [scores (->> lines
                     (map check-syntax)
                     (filter #(= (:status %) :incomplete))
                     (map #(->> % :state complete score))
                     (sort)
                     (into []))]
     (nth scores (/ (count scores) 2)))))
