(ns advent-of-code.day-14
  (:require [clojure.spec :as s]
            [clojure.string :refer [split]]))

(def raw-input "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.\nCupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.\nPrancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.\nDonner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.\nDasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.\nComet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.\nBlitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.\nVixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.\nDancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds.")
(def input (split raw-input #"\n"))

(def reindeer-pattern #"([^ ]+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.")

(s/def ::name string?)
(s/def ::speed nat-int?)
(s/def ::flight-time nat-int?)
(s/def ::rest-time nat-int?)
(s/def ::reindeer (s/keys :req [::name ::speed ::flight-time ::rest-time]))

(defn parse-line
  [line]
  (let [[[_ name speed flight-time rest-time]] (re-seq reindeer-pattern line)]
    {::name        name
     ::speed       (Integer/parseInt speed)
     ::flight-time (Integer/parseInt flight-time)
     ::rest-time   (Integer/parseInt rest-time)}))

(s/def ::state #{:flying :resting})
(s/def ::current-state-time nat-int?)
(s/def ::distance nat-int?)
(s/def ::competitor (s/keys :req [::reindeer ::state ::current-state-time ::distance]))

(s/def ::race (s/coll-of ::competitor))

(defn make-race [reindeers]
  (map (fn [reindeer]
         {::reindeer           reindeer
          ::state              :flying
          ::current-state-time 0
          ::distance           0})
       reindeers))

(defn race-tick [race]
  (map (fn [competitor]
         (condp = (competitor ::state)
           :flying (if (< (competitor ::current-state-time)
                          (get-in competitor [::reindeer ::flight-time]))
                     (-> competitor
                         (update-in [::distance] + (get-in competitor [::reindeer ::speed]))
                         (update-in [::current-state-time] inc))
                     (-> competitor
                         (assoc ::state :resting)
                         (assoc ::current-state-time 1)))

           :resting (if (< (competitor ::current-state-time)
                           (get-in competitor [::reindeer ::rest-time]))
                      (update-in competitor [::current-state-time] inc)
                      (-> competitor
                          (assoc ::state :flying)
                          (assoc ::current-state-time 1)
                          (update-in [::distance] + (get-in competitor [::reindeer ::speed]))))))
       race))

(defn run-race [reindeers num-seconds]
  (let [race (make-race reindeers)]
    (nth (iterate race-tick race) num-seconds)))

(def reindeers (map parse-line input))

(comment
  (as-> reindeers $
        (run-race $ 2503)
        (map ::distance $)
        (apply max $))

  (run-race reindeers 1)

  (parse-line (first input))

  )