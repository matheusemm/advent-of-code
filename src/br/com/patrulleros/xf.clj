(ns br.com.patrulleros.xf
  (:import (java.util ArrayDeque)))

(defn sliding
  ([^long n]
   (sliding n n))
  ([^long n ^long step]
   (fn [rf]
     (let [deque (ArrayDeque. n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (.add deque input)
          (if (= n (.size deque))
            (let [v (vec (.toArray deque))]
              (dotimes [_ step] (.removeFirst deque))
              (rf result v))
            result)))))))
