(ns advent-of-code.day-9
  (:require [clojure.spec :as s]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.set :refer [union]]
            [clojure.string :refer [split]]))

; traveling santa problem

(s/def ::location string?)
(s/def ::distance (s/conformer edn/read-string))
(s/def ::segment (s/cat :origin ::location
                        :to #{"to"}
                        :destination ::location
                        :equals #{"="}
                        :cost ::distance))

(def raw-inputs "Faerun to Norrath = 129\nFaerun to Tristram = 58\nFaerun to AlphaCentauri = 13\nFaerun to Arbre = 24\nFaerun to Snowdin = 60\nFaerun to Tambi = 71\nFaerun to Straylight = 67\nNorrath to Tristram = 142\nNorrath to AlphaCentauri = 15\nNorrath to Arbre = 135\nNorrath to Snowdin = 75\nNorrath to Tambi = 82\nNorrath to Straylight = 54\nTristram to AlphaCentauri = 118\nTristram to Arbre = 122\nTristram to Snowdin = 103\nTristram to Tambi = 49\nTristram to Straylight = 97\nAlphaCentauri to Arbre = 116\nAlphaCentauri to Snowdin = 12\nAlphaCentauri to Tambi = 18\nAlphaCentauri to Straylight = 91\nArbre to Snowdin = 129\nArbre to Tambi = 53\nArbre to Straylight = 40\nSnowdin to Tambi = 15\nSnowdin to Straylight = 99\nTambi to Straylight = 70")
(def inputs (split raw-inputs #"\n"))
(def segments (map (fn [input]
                     (s/conform ::segment
                                (split input #" ")))
                   inputs))

(def cost-map
  (into {}
        (map (fn [segment]
               [#{(segment :origin) (segment :destination)}
                (segment :cost)])
             segments)))

(def all-locations
  (apply union (keys cost-map)))

(defn route-cost [route]
  (reduce (fn [state curr-location]
            {:last-location curr-location
             :cost          (+ (state :cost)
                               (cost-map #{(state :last-location) curr-location}))})
          {:last-location (first route)
           :cost          0}
          (rest route)))

(def all-routes
  (into {}
        (map (fn [route]
               [route (route-cost route)])
             (permutations all-locations))))

(def max-cost-route
  (apply max-key
         (fn [[_ state]]
           (state :cost))
         all-routes))

(comment
  all-routes

  max-cost-route

  (rest all-locations)
  (as-> "London to Dublin = 464" $
        (split $ #" ")
        (s/conform ::segment $)
        )

  (permutations all-locations)

  cost-map

  all-locations
  )

