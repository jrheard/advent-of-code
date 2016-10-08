(ns advent-of-code.day-10
  (:require [clojure.spec :as s]
            [clojure.string :refer [split]]))

; http://adventofcode.com/2015/day/10

(s/def ::digit (s/and nat-int? #(< % 10)))
(s/def ::run (s/& (s/+ ::digit)
                  #(= (count (set %)) 1)))
(s/def ::sequence (s/* ::run))

(def start [1])

(defn look-and-say [look-and-say-seq]
  (println (count look-and-say-seq))
  (let [parsed-sequence (s/conform ::sequence look-and-say-seq)]
    (mapcat (fn [run]
              [(count run) (first run)])
            parsed-sequence)))

(comment
  (-> look-and-say
      (iterate start)
      (nth 20)
      count)

  (s/exercise ::run)

  (s/conform ::sequence [1])

  (look-and-say start)

  (take 40 (iterate look-and-say start))
  )
