(ns advent-of-code.2022.day15
  "Fifteenth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn dist
  "Function to compute the Manhattan distance between the points."
  [[x0 y0] [x1 y1]]
  (+ (Math/abs (- x0 x1)) (Math/abs (- y0 y1))))

(defn parse
  "Function to take a description line from the sensor and mae it into a
  useable data structure."
  [s]
  (let [[_ sx sy bx by] (map parse-int (re-matches #"Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)" s))
        r (dist [sx sy] [bx by])]
    {:sensor [sx sy] :beacon [bx by] :radius r}))

(def puzzle
  "This is the input of sensor reading from the probes sent into the cave."
  (-> (slurp "resources/2022/input/day15.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2022/input/day15test.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(defn one
  "Function to find the number of places that a beacon cannot be on a single
  line. This is a lot faster if we use the distance to calculate the only
  possible positions, and then just exclude those that are occupiued by the
  existing beacons."
  [& [coll]]
  (let [data puzzle
        beacs (set (map :beacon data))
        tgt 2000000
        near (filter #(<= (- tgt (:radius %)) (second (:sensor %)) (+ tgt (:radius %))) data)
        hits (atom [])]
    (doseq [sb near
            :let [[sx sy] (:sensor sb)
                  r (:radius sb)
                  dx (- r (Math/abs (- tgt sy)))]
            x (range (- sx dx) (inc (+ sx dx)))
            :when (not (beacs [x tgt]))]
      (swap! hits conj [x tgt]))
    (swap! hits distinct)
    {:count (count @hits)}))

(defn outside?
  "Function look at the point '[x y]' and see if it's inside the limits of
  'lim', and soutside the sensor radii of all the sensors in the 'sbs'
  sequence."
  [sbs [x y] lim]
  (if (and (<= 0 x lim) (<= 0 y lim))
    (not-any? #(or (<= (dist [x y] (:sensor %)) (:radius %)) (= [x y] (:beacon %))) sbs)))

(defn two
  "Function to find the one point that outside all the sensor ranges that
  is not a beacon... we do this by walking the perimeters of each sensor
  range, and then check if the point is outside all of the sensors. With
  the fail-fast on not-any? we should be about as efficient as possible."
  [& [coll]]
  (let [data puzzle
        lim 4000000]
    (first
      (for [sb data
            :let [[sx sy] (:sensor sb)
                  r (inc (:radius sb))]
            i (range r)
            [tx ty] [[(+ sx i)       (- (+ sy i) r)]
                     [(+ (- sx i) r) (+ sy i)]
                     [(- sx i)       (+ (- sy i) r)]
                     [(- (+ sx i) r) (- sy i)]]
            :when (outside? data [tx ty] lim)]
        {:loc [tx ty] :freq (+ (* tx 4000000) ty)}))))
