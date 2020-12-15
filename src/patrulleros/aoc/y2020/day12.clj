(ns patrulleros.aoc.y2020.day12
  (:require [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day12.txt")))

(def example
  ["F10"
   "N3"
   "F7"
   "R90"
   "F11"])

(def directions
  {0 \E
   90 \S
   180 \W
   270 \N})

(defn create-ship []
  {:direction 0
   :position [0 0]
   :waypoint [10 1]})

(defn execute-instruction-p1 [ship instruction]
  (let [action (first instruction)
        value (Integer/parseInt (subs instruction 1))]
    (case action
      (\N \S \E \W)
      (move-nsew ship action value)

      \F
      (move-f ship value)

      (\L \R)
      (rotate ship action value))))

(defn manhattan-distance [ship]
  (let [[x y] (:position ship)]
    (+ (Math/abs x) (Math/abs y))))

(defn execute-instruction [ship instruction move-nsew move-f rotate]
  (let [action (first instruction)
        value (Integer/parseInt (subs instruction 1))]
    (case action
      (\N \S \E \W)
      (move-nsew ship action value)

      \F
      (move-f ship value)

      (\L \R)
      (rotate ship action value))))

(defn move-nsew-p1 [ship direction value]
  (let [value (if (#{\S \W} direction) (- value) value)]
    (if (#{\E \W} direction)
      (update-in ship [:position 0] + value)
      (update-in ship [:position 1] + value))))

(defn move-f-p1 [ship value]
  (let [direction (directions (:direction ship))]
    (move-nsew ship direction value)))

(defn rotate-p1 [ship direction value]
  (let [value (if (= direction \L) (- value) value)
        direction (+ (:direction ship) value)]
    (assoc ship :direction (mod direction 360 ))))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([instructions]
   (let [ship (reduce #(execute-instruction %1 %2 move-nsew-p1 move-f-p1 rotate-p1)
                      (create-ship)
                      instructions)]
     (manhattan-distance ship))))

(defn move-waypoint [ship direction value]
  (let [value (if (#{\S \W} direction) (- value) value)]
    (if (#{\E \W} direction)
      (update-in ship [:waypoint 0] + value)
      (update-in ship [:waypoint 1] + value))))

(defn rotate-waypoint [{:keys [waypoint] :as ship} direction value]
  (let [value (-> (if (= direction \L) (- value) value)
                  (mod 360))
        [wx wy] waypoint]
    (case value
      90
      (assoc ship :waypoint [wy (- wx)])

      180
      (assoc ship :waypoint [(- wx) (- wy)])

      270
      (assoc ship :waypoint [(- wy) wx]))))

(defn move-ship [{:keys [waypoint] :as ship} value]
  (let [[wx wy] waypoint
        dx (* wx value)
        dy (* wy value)]
    (-> ship
        (update-in [:position 0] + dx)
        (update-in [:position 1] + dy))))

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([instructions]
   (let [ship (reduce #(execute-instruction %1 %2 move-waypoint move-ship rotate-waypoint)
                      (create-ship)
                      instructions)]
     (manhattan-distance ship))))

(comment
  (assert (= 998 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 71586 (solve-p2)) "Part 2 solution is wrong."))
