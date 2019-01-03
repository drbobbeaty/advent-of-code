(ns advent-of-code.2017.day22
  "Twenty-second day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.util :refer [split]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the map of the grid computing network from the puzzle."
  (-> (slurp "resources/2017/input/day22.txt")
    (split "\n")
    (as-> s (remove cs/blank? s))
    (as-> s (mapv vec s))))

(def sample
  "This is the sample grid network for the examples in the puzzle."
  (->> ["..#"
        "#.."
        "..."]
    (mapv vec)))

(defn gget
  "Function to get the state of the node in the (atom) grid at row `r`,
  and column `c`."
  [g r c]
  (-> (nth @g r [])
    (nth c nil)))

(defn gset
  "Function to set the state of the node at row `r`, and column `c`, to
  the value `v` in the grid atom 'g'."
  [g r c v]
  (reset! g (-> (nth @g r [])
              (assoc c v)
              (->> (assoc @g r)))))

(def ^:private infections
  "This atom is for counting all the infections that occur during a series
  of movements."
  (atom 0))

(defn move
  "Function to take the atom that is the grid, and the virus carrier, and
  move the virus carrier one 'step' by turning it, changing the infection
  state of the node it's on, and then moving it to the next node in the
  grid. If this ends up being on an 'edge' of the grid, we automatically
  add 5 rows at the top and bottom, and 5 columns on the left and right to
  keep the grid balanced."
  [gcc {[r c] :pos d :dir :as vc} & [chg-fcn]]
  (let [ncf (or chg-fcn (fn [c] (case c
                                  \. \#
                                  \# \.)))
        h (count @gcc)
        w (count (first @gcc))
        cns (gget gcc r c)
        nns (ncf cns)
        d' (case d
             \^ (case cns
                  \. \<
                  \w \^
                  \# \>
                  \f \v)
             \v (case cns
                  \. \>
                  \w \v
                  \# \<
                  \f \^)
             \< (case cns
                  \. \v
                  \w \<
                  \# \^
                  \f \>)
             \> (case cns
                  \. \^
                  \w \>
                  \# \v
                  \f \<))
        [r' c'] (case d'
                  \^ [(dec r) c]
                  \v [(inc r) c]
                  \< [r (dec c)]
                  \> [r (inc c)])]
    (if (= \# nns) (swap! infections inc))
    (gset gcc r c nns)
    (if (and (< 0 r' (dec h)) (< 0 c' (dec w)))
      {:pos [r' c'] :dir d'}
      (let [rbuf (repeat 5 (vec (repeat (+ w 10) \.)))
            cbuf (repeat 5 \.)]
        (reset! gcc (vec (concat rbuf (map #(vec (concat cbuf % cbuf)) @gcc) rbuf)))
        {:pos [(+ r' 5) (+ c' 5)] :dir d'}))))

(defn one
  "Function to start with the puzzle input as the compute grid state, and after
  10,000 moves, return the number of infections the virus carrier did in it's
  trip around the grid."
  [& [gcc]]
  (let [grd (atom (or gcc puzzle))
        h (count @grd)
        w (count (first @grd))]
    (-> (loop [vc {:pos [(quot w 2) (quot h 2)] :dir \^}
               cnt 0]
          (if (< cnt 10000)
            (recur (move grd vc) (inc cnt))
            vc))
      (assoc :infections @infections))))

(defn two
  "Function to start with the puzzle input as the compute grid state, and after
  10  mil moves, return the number of infections the virus carrier did in it's
  trip around the grid. Also, use the new rules for the virus carrier where the
  grid has weakened, and flagged states in addition to the clean and infected."
  [& [gcc]]
  (let [grd (atom (or gcc puzzle))
        h (count @grd)
        w (count (first @grd))
        ncf (fn [c] (case c
                      \. \w
                      \w \#
                      \# \f
                      \f \.))]
    (-> (loop [vc {:pos [(quot w 2) (quot h 2)] :dir \^}
               cnt 0]
          (if (< cnt 10000000)
            (recur (move grd vc ncf) (inc cnt))
            vc))
      (assoc :infections @infections))))

(defn yoyo
  "Function to just allow us to test some components we're using to build
  things up for the puzzle."
  [& [gcc]]
  (let [grd (atom (or gcc sample))
        h (count @grd)
        w (count (first @grd))]
    (-> (loop [vc {:pos [(quot w 2) (quot h 2)] :dir \^}
               cnt 0]
          (if (< cnt 100)
            (recur (move grd vc) (inc cnt))
            vc))
      (assoc :grid @grd :infections @infections))))
