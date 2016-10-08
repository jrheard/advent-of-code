(ns advent-of-code.day-10
  (:require [clojure.spec :as s]
            [clojure.string :refer [split]]))

(s/def ::digit (s/and nat-int? #(< % 10)))
(s/def ::run (s/& (s/+ ::digit)
                    #(= (count (set %)) 1)))
(s/def ::sequence (s/* ::run))

(def start [1])

(defn look-and-say [look-and-say-seq]

  )

(comment
  (s/exercise ::run)

  (s/conform ::sequence [1 1 2 2 2 2 1 3 4 4 4 2])
  )
