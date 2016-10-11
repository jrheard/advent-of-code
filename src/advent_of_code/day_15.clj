(ns advent-of-code.day-15
  (:require [clojure.spec :as s]
            [clojure.math.combinatorics :refer [selections] :as combo]
            [clojure.string :refer [split]]))

(def raw-input "Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2\nSprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9\nCandy: capacity -1, durability 0, flavor 4, texture 0, calories 1\nChocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8")
(def input (split raw-input #"\n"))

(def ingredient-re #"([^ ]+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")

(s/def ::capacity int?)
(s/def ::durability int?)
(s/def ::flavor int?)
(s/def ::texture int?)
(s/def ::calories int?)
(s/def ::name string?)
(s/def ::ingredient (s/keys :req [::name ::capacity ::durability ::flavor ::texture ::calories]))

(defn parse-ingredient [line]
  (let [[[_ name capacity durability flavor texture calories]] (re-seq ingredient-re line)
        p #(Integer/parseInt %)]
    {::name       name
     ::capacity   (p capacity)
     ::durability (p durability)
     ::flavor     (p flavor)
     ::texture    (p texture)
     ::calories   (p calories)}))

(def ingredients (map parse-ingredient input))

(s/def ::amount nat-int?)
(s/def ::cookie (s/coll-of (s/tuple ::ingredient ::amount)))

(defn score [cookie]
  (apply *
         (map (fn [attribute]
                (let [attribute-total (apply +
                                             (map (fn [[ingredient amount]]
                                                    (* (ingredient attribute) amount))
                                                  cookie))]
                  (if (> attribute-total 0)
                    attribute-total
                    0)))
              #{::capacity ::durability ::flavor ::texture})))

(defn part-1
  []
  (apply max (for [ingredient-amounts (selections (range 101) (count ingredients))
                   :when (= (apply + ingredient-amounts)
                            100)]
               (score (map vector
                           ingredients
                           ingredient-amounts)))))


(comment
  (part-1)



  )