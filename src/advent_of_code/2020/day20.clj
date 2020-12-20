(ns advent-of-code.2020.day20
  "Twentieth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split un-seq]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn edges
  "Function to take a tile, and return a map of the possible edges for the
  tile."
  [tile]
  (let [t (first tile)
        b (last tile)
        r (map last tile)
        l (map first tile)]
    {:top t :top-rev (apply str (reverse t))
     :bot b :bot-rev (apply str (reverse b))
     :r (apply str r) :r-rev (apply str (reverse r))
     :l (apply str l) :l-rev (apply str (reverse l))}))

(defn parse
  "Function to parse the input into a collection of tiles suitable for easier
  manipulation."
  [coll]
  (loop [src coll
         ans (transient [])]
    (if-let [tl (first src)]
      (let [t (second (re-matches #"^Tile (\d+):$" (trim tl)))
            tile (take 10 (rest src))]
        (recur (drop 12 src) (conj! ans {:id (parse-int t) :pic tile :edges (edges tile)})))
      (persistent! ans))))

(def puzzle
  "This is the input of"
  (-> (slurp "resources/2020/input/day20.txt")
      (trim)
      (split #"\n")
      (parse)))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2020/input/day20test.txt")
      (trim)
      (split #"\n")
      (parse)))

(defn edge-fits
  "Function to return all the edge-matches for the two tiles a and b. This
  will be of the form: [id edge id edge] and will represent a match."
  [a b]
  (first
    (for [[ak av] (:edges a) [bk bv] (:edges b) :when (= av bv)]
      [(:id a) ak (:id b) bk])))

(defn fit
  "Function to find all the edge fits on all the tiles in the image sequence."
  [coll]
  (for [t coll :let [rst (remove #(= t %) coll)]]
    (assoc t :hits (remove nil? (map #(edge-fits t %) rst)))))

(defn one
  "Function to find the four corner pieces as they are the only ones with
  just two edge matches, and then we have to multiply their ids to return
  the answer."
  [& [coll]]
  (->> (fit puzzle)
       (filter #(= 2 (count (:hits %))))
       (map :id)
       (apply *)))

(defn two
  "Function to "
  [& [coll]]
  )
