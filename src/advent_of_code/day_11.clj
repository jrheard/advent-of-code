(ns advent-of-code.day-11
  (:require [clojure.spec :as s]
            [clojure.string :refer [split]]))

(s/def ::letter (s/and char?
                       #(re-seq #"[a-z]" (str %))))
(s/def ::password (s/coll-of ::letter :count 8))

(def input (vec "hepxcrrq"))

(defn increment-pass
  [password]
  (let [last-letter (last password)
        everything-but-last (vec (take (dec (count password))
                                       password))]
    (if (= last-letter \z)
      (conj (increment-pass everything-but-last) \a)
      (conj everything-but-last (char (inc (int last-letter)))))))

(defn valid-password?
  [password]
  true)

(defn find-next-password
  [password]
  (let [new-pass (increment-pass password)]
    (if (valid-password? new-pass)
      new-pass
      (find-next-password new-pass))))

(comment

  (apply str
         (conj [\a \b \c] (char (inc (int \d))))
         )



  (apply str \a [\b \c \d])

  (char (+ (int \a) 1))

  (nth (iterate find-next-password input) 20)

  (s/exercise ::password)
  )
