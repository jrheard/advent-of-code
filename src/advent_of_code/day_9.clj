(ns advent-of-code.day-9
  (:require [clojure.spec :as s]
            [clojure.edn :as edn]
            [clojure.string :refer [split]]))

; traveling santa problem

(s/def ::location string?)
(s/def ::distance (s/conformer edn/read-string))
(s/def ::segment (s/cat :origin ::location
                        :to #{"to"}
                        :destination ::location
                        :equals #{"="}
                        :cost ::distance))

(def raw-inputs "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141")
(def inputs (split raw-inputs #"\n"))
(def segments (map (fn [input]
                     (s/conform ::segment
                                (split input #" ")))
                   inputs))

(def cost-map (into {}
                    (map (fn [segment]
                           [#{(segment :origin) (segment :destination)}
                            (segment :cost)])
                         segments)))


(comment
  (as-> "London to Dublin = 464" $
        (split $ #" ")
        (s/conform ::segment $)
        )

  cost-map
  )

