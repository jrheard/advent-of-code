(ns advent-of-code.day-13
  (:require [clojure.spec :as s]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :refer [split]]))

(def raw-input "Alice would lose 2 happiness units by sitting next to Bob.\nAlice would lose 62 happiness units by sitting next to Carol.\nAlice would gain 65 happiness units by sitting next to David.\nAlice would gain 21 happiness units by sitting next to Eric.\nAlice would lose 81 happiness units by sitting next to Frank.\nAlice would lose 4 happiness units by sitting next to George.\nAlice would lose 80 happiness units by sitting next to Mallory.\nBob would gain 93 happiness units by sitting next to Alice.\nBob would gain 19 happiness units by sitting next to Carol.\nBob would gain 5 happiness units by sitting next to David.\nBob would gain 49 happiness units by sitting next to Eric.\nBob would gain 68 happiness units by sitting next to Frank.\nBob would gain 23 happiness units by sitting next to George.\nBob would gain 29 happiness units by sitting next to Mallory.\nCarol would lose 54 happiness units by sitting next to Alice.\nCarol would lose 70 happiness units by sitting next to Bob.\nCarol would lose 37 happiness units by sitting next to David.\nCarol would lose 46 happiness units by sitting next to Eric.\nCarol would gain 33 happiness units by sitting next to Frank.\nCarol would lose 35 happiness units by sitting next to George.\nCarol would gain 10 happiness units by sitting next to Mallory.\nDavid would gain 43 happiness units by sitting next to Alice.\nDavid would lose 96 happiness units by sitting next to Bob.\nDavid would lose 53 happiness units by sitting next to Carol.\nDavid would lose 30 happiness units by sitting next to Eric.\nDavid would lose 12 happiness units by sitting next to Frank.\nDavid would gain 75 happiness units by sitting next to George.\nDavid would lose 20 happiness units by sitting next to Mallory.\nEric would gain 8 happiness units by sitting next to Alice.\nEric would lose 89 happiness units by sitting next to Bob.\nEric would lose 69 happiness units by sitting next to Carol.\nEric would lose 34 happiness units by sitting next to David.\nEric would gain 95 happiness units by sitting next to Frank.\nEric would gain 34 happiness units by sitting next to George.\nEric would lose 99 happiness units by sitting next to Mallory.\nFrank would lose 97 happiness units by sitting next to Alice.\nFrank would gain 6 happiness units by sitting next to Bob.\nFrank would lose 9 happiness units by sitting next to Carol.\nFrank would gain 56 happiness units by sitting next to David.\nFrank would lose 17 happiness units by sitting next to Eric.\nFrank would gain 18 happiness units by sitting next to George.\nFrank would lose 56 happiness units by sitting next to Mallory.\nGeorge would gain 45 happiness units by sitting next to Alice.\nGeorge would gain 76 happiness units by sitting next to Bob.\nGeorge would gain 63 happiness units by sitting next to Carol.\nGeorge would gain 54 happiness units by sitting next to David.\nGeorge would gain 54 happiness units by sitting next to Eric.\nGeorge would gain 30 happiness units by sitting next to Frank.\nGeorge would gain 7 happiness units by sitting next to Mallory.\nMallory would gain 31 happiness units by sitting next to Alice.\nMallory would lose 32 happiness units by sitting next to Bob.\nMallory would gain 95 happiness units by sitting next to Carol.\nMallory would gain 91 happiness units by sitting next to David.\nMallory would lose 66 happiness units by sitting next to Eric.\nMallory would lose 75 happiness units by sitting next to Frank.\nMallory would lose 99 happiness units by sitting next to George.")
(def input (split raw-input #"\n"))

(def instruction-pattern #"([^ ]+) would (gain|lose) (\d+) happiness units by sitting next to ([^ ]+).")

(s/def ::person string?)
(s/def ::other-person string?)
(s/def ::change-type #{:gain :lose})
(s/def ::happiness number?)

(s/def ::instruction (s/keys :req [::person ::change-type ::happiness ::other-person]))

(defn parse-instruction [instruction-string]
  (let [[[_ person change-type happiness other-person]] (re-seq instruction-pattern instruction-string)]
    {::person       person
     ::change-type  (keyword change-type)
     ::happiness    (Integer/parseInt happiness)
     ::other-person other-person}))

(def instructions (map parse-instruction input))

(def all-other-people (set (map ::person instructions)))

(def instructions-incliuding-yours
  (apply conj
         instructions
         (mapcat (fn [person]
                   [{::person "You" ::change-type :gain ::happiness 0 ::other-person person}
                    {::person person ::change-type :gain ::happiness 0 ::other-person "You"}])
                 all-other-people)))

(def happiness-change-by-pair
  (into {}
        (map (fn [instruction]
               [[(instruction ::person) (instruction ::other-person)]
                (if (= (instruction ::change-type) :gain)
                  (instruction ::happiness)
                  (- (instruction ::happiness)))]))
        instructions-incliuding-yours))

(def all-possible-arrangements
  (permutations (conj all-other-people "You")))

(defn happiness-changes-for-arrangement
  [arrangement]
  (let [nearly-all-happiness-changes ((reduce (fn [state current-person]
                                                (-> state
                                                    (update :happiness-changes
                                                            conj
                                                            (happiness-change-by-pair [(state :last-person)
                                                                                       current-person]))
                                                    (update :happiness-changes
                                                            conj
                                                            (happiness-change-by-pair [current-person
                                                                                       (state :last-person)]))
                                                    (assoc :last-person current-person)))
                                              {:last-person       (first arrangement)
                                               :happiness-changes []}
                                              (rest arrangement))
                                       :happiness-changes)]
    (conj nearly-all-happiness-changes
          (happiness-change-by-pair [(first arrangement) (last arrangement)])
          (happiness-change-by-pair [(last arrangement) (first arrangement)]))))

(comment
  (apply max
         (map (fn [arrangement]
                (apply +
                       (happiness-changes-for-arrangement arrangement)))
              all-possible-arrangements))

  (second all-possible-arrangements)
  (happiness-changes-for-arrangement (nth all-possible-arrangements 4))

  )
