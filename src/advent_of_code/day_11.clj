(ns advent-of-code.day-11
  (:require [clojure.spec :as s]
            [clojure.string :refer [split]]))

(s/def ::letter (s/and char?
                       #(re-seq #"[a-z]" (str %))))
(s/def ::password (s/coll-of ::letter :count 8))

(s/def ::straight (s/& (s/cat :first ::letter
                              :second ::letter
                              :third ::letter)
                       #(and (= (int (% :first))
                                (dec (int (% :second))))
                             (= (int (% :second))
                                (dec (int (% :third)))))))

(s/def ::password-with-straight (s/cat :some-letters (s/* ::letter)
                                       :straight ::straight
                                       :some-other-letters (s/* ::letter)))


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

  (s/valid? ::password-with-straight (vec "hijklmmn"))


  (apply str \a [\b \c \d])

  (char (+ (int \a) 1))

  (nth (iterate find-next-password input) 20)

  (s/exercise ::password)
  )
