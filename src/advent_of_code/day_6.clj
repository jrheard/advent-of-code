(ns advent-of-code.day-6
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.string :refer [split join]]))

#_(def lights
    (to-array-2d (for [_ (range 1000)]
                   (for [_ (range 1000)]
                     0))))

(s/def ::x nat-int?)
(s/def ::y nat-int?)
(s/def ::point (s/keys :req [::x ::y]))

(s/def ::instruction-type #{:turn-on :toggle :turn-off})
(s/def ::start ::point)
(s/def ::end ::point)
(s/def ::instruction (s/keys :req [::instruction-type ::start ::end]))

(defn parse-point [point-str]
  (let [[x y] (split point-str #",")]
    {::x (read-string x)
     ::y (read-string y)}))

(defn parse-instruction [instruction]
  (let [words (split instruction #" ")
        coord-words (take-last 3 words)
        verb (drop-last 3 words)]
    {::instruction-type (keyword (join "-" verb))
     ::start            (parse-point (first coord-words))
     ::end              (parse-point (last coord-words))}))

(comment
  (map first (s/exercise ::instruction))

  (alength lights)
  (aget lights 999 999)

  (join "-" ["toggle"])

  (drop-last 3 (split "turn off 499,499 through 500,500" #" "))

  (parse-instruction "toggle 499,499 through 500,500")

  )
