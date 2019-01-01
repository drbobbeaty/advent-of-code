(ns advent-of-code.2017.day20
  "Twentieth day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.path :refer [dist]]
            [advent-of-code.util :refer [parse-int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn code-it
  "Function to take the input from the puzzle and parse it into a reasonable
  format so that it's easy to work with."
  [s]
  (cond
    (string? s) (let [pts (map parse-int (drop 1 (re-matches #"^p=<([-\d]+),([-\d]+),([-\d]+)>, v=<([-\d]+),([-\d]+),([-\d]+)>, a=<([-\d]+),([-\d]+),([-\d]+)>$" s)))]
                  {:pos (vec (take 3 pts))
                   :vel (vec (take 3 (drop 3 pts)))
                   :acc (vec (take-last 3 pts))})
    (coll? s)   (for [[i p] (map vector (range) (map code-it s))]
                  (assoc p :particle i))
    :else       s))

(def puzzle
  "This is the list of particle characteristics that are in the storm."
  (-> (slurp "resources/2017/input/day20.txt")
    (cs/split #"\n")
    (code-it)))

(defn move
  "Function to move a particle one 'step' in the progression, by first
  updating the velocity and then the position. We need to use the updated
  velocity on the position calc, as it's in the rules."
  [{i :particle [x y z] :pos [vx vy vz] :vel [ax ay az] :acc :as ptc}]
  (let [vx' (+ vx ax)
        vy' (+ vy ay)
        vz' (+ vz az)
        x' (+ x vx')
        y' (+ y vy')
        z' (+ z vz')]
    (assoc ptc :pos [x' y' z'] :vel [vx' vy' vz'])))

(defn collisions
  "Function to collect all the collisions between the particles passed in.
  This is simply looking for the duplicated positions of the particles,
  and then producing a list of all those [x y z] tuples, and returning it."
  [pds]
  (for [[k v] (frequencies (map :pos pds))
        :when (< 1 v)]
    k))

(defn one
  "Function to find the 'long-term' closest point to the origin, and we
  have to do this by just running 1000 steps on all the points, and see
  if there emerges a clear point that is consistently closest to the origin."
  []
  (let [df (fn [p] (dist (:pos p) [0 0 0]))]
    (loop [pds puzzle
           cnt 0]
      (let [nxt (map #(assoc % :dist (df %)) (map move pds))
            cls (->> (sort-by :dist nxt)
                  (map :particle)
                  (take 10)
                  (vec))]
        (infof "[%s] closest: %s" cnt cls)
        (if (< cnt 1000)
          (recur nxt (inc cnt)))))))

(defn two
  "Function to account for collisions between the particles, and remove
  those particles taking part in collisions from the list, and keep going
  with the rest. The point is to find out how many are left after all the
  collisions that will take place, have taken place."
  []
  (let [df (fn [p] (dist (:pos p) [0 0 0]))]
    (loop [pds puzzle
           cnt 0]
      (let [nxt (map move pds)
            bang (set (collisions nxt))
            nxt' (remove #(bang (:pos %)) nxt)]
        (infof "[%s] particles: %s" cnt (count nxt'))
        (if (< cnt 1000)
          (recur nxt' (inc cnt)))))))
