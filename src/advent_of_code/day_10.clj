(ns advent-of-code.day-10
  (:require [clojure.spec :as s]
            [clojure.string :refer [split]]))

; http://adventofcode.com/2015/day/10

(def start [1 3 2 1 1 3 1 1 1 2])

; super pretty look-and-say implementation with spec; too slow though :'(

(s/def ::digit (s/and nat-int? #(< % 10)))
(s/def ::run (s/& (s/+ ::digit)
                  #(= (count (set %)) 1)))
(s/def ::sequence (s/* ::run))

(defn look-and-say
  [look-and-say-seq]
  (println (count look-and-say-seq))
  (let [parsed-sequence (s/conform ::sequence look-and-say-seq)]
    (mapcat (fn [run]
              [(count run) (first run)])
            parsed-sequence)))

; faster implementation with reduce

(defn fast-look-and-say
  [look-and-say-seq]
  (println (count look-and-say-seq))
  (let [unprocessed-state (reduce (fn [state digit]
                                    (if (= digit (state :run-digit))
                                      (update-in state [:run-count] inc)

                                      (-> state
                                          (update-in [:new-seq]
                                                     conj
                                                     (state :run-count)
                                                     (state :run-digit))
                                          (assoc :run-count 1)
                                          (assoc :run-digit digit))))
                                  {:new-seq   []
                                   :run-count 1
                                   :run-digit (first look-and-say-seq)}
                                  (rest look-and-say-seq))]
    (conj (unprocessed-state :new-seq)
          (unprocessed-state :run-count)
          (unprocessed-state :run-digit))))

(comment
  (fast-look-and-say (fast-look-and-say start))

  (-> fast-look-and-say
      (iterate start)
      (nth 40)
      count
      )

  (conj [2 3 4] 1 2)

  (s/exercise ::run)

  (s/conform ::sequence [1])

  (look-and-say start)

  (take 40 (iterate look-and-say start))
  )
