(ns advent-of-code.day-11
  (:require [advent-of-code.day-5 :refer [contains-a-non-overlapping-pair?]]
            [clojure.spec :as s]
            [clojure.string :refer [split]]))

; started with a spec implementation, but it was waaaay too slow :'(
; makes sense, it wasn't appropriate here, this is a job for regular regular expressions

(s/def ::letter (s/and char?
                       #(re-seq #"[a-hjkmnp-z]" (str %))))

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

(s/def ::pair (s/& (s/cat :first ::letter
                          :second ::letter)
                   #(= (% :first) (% :second))))

(s/def ::password-with-two-pairs (s/and (s/cat :some-letters (s/* ::letter)
                                               :first-pair ::pair
                                               :letters-between-pairs (s/* ::letter)
                                               :second-pair ::pair
                                               :some-other-letters (s/* ::letter))))

(s/def ::password (s/and (s/coll-of ::letter :count 8)
                         ::password-with-straight
                         ::password-with-two-pairs))

(def input "hepxcrrq")

(defn increment-pass
  [password]
  (let [last-letter (last password)
        everything-but-last (vec (take (dec (count password))
                                       password))]
    (if (= last-letter \z)
      (conj (increment-pass everything-but-last) \a)
      (conj everything-but-last (char (inc (int last-letter)))))))

(def straights
  (map #(apply str %) (partition 3 1 (map char (range 97 123)))))

(def straights-re
  (re-pattern (apply str (interleave straights (repeat "|")))))

(defn fast-valid-password?
  [password]
  (and
    (re-seq straights-re password)
    (not (re-seq #"[iol]" password))

    ;; this function is buggy
    (contains-a-non-overlapping-pair? password)

    (= (count password) 8)))

(defn find-next-password
  [password]
  (println password)
  ; turn into vec in increment, then back into string
  (let [new-pass (apply str (increment-pass (vec password)))]
    (if (fast-valid-password? new-pass)
      new-pass
      (recur new-pass))))

(comment
  straights

  (contains-a-non-overlapping-pair? "abcdegab")

  (find-next-password "abcdefgh")

  (apply str (interleave straights (repeat "|")))

  (re-find #"abc|bcd" "cdef")

  (s/valid? ::password (vec "abcdaaa"))

  (re-seq #"[a-hj-z]" "j")

  (nth (iterate find-next-password input) 20)

  (s/exercise ::password)
  )
