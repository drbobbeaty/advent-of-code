(ns advent-of-code.2017.day03
  "Third day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(defn spiral
  "Function to calculate the location of the provided target number on the
  spiral as defined in the puzzle, and return it's coordinates on that spiral,
  and the distance from the center of the spiral."
  [tgt]
  (let [[d e c] (->> (range)
                  (map (fn [i] [(inc i) (inc (* 2 i))]))
                  (map (fn [[d e]] [d e (* e e)]))
                  (drop-while #(< (last %) tgt))
                  (first))
        esz (dec e)]
    (-> (cond
          (< (- c esz) tgt)       ;; bottom
            [(- (quot e 2) (- c tgt)) (dec d)]
          (< (- c (* 2 esz)) tgt) ;; left
            [(dec d) (- (quot e 2) (- (- c esz) tgt))]
          (< (- c (* 3 esz)) tgt) ;; top
            [(- (quot e 2) (- (- c (* 2 esz)) tgt)) (dec d)]
          :else                   ;; right
            [(dec d) (- (quot e 2) (- (- c (* 3 esz)) tgt))])
      (as-> p {:target tgt :point p :dist (apply + p)}))))

(defn near?
  "Predicate function to look at the two points and see if they are
  neighbors - meaning they are only one unit distance in any direction."
  [[x y] [a b]]
  (and x y a b (<= (Math/abs (- x a)) 1) (<= (Math/abs (- y b)) 1)))

(defn one
  "Function to determine the location of the target point in the spiral, and
  the distance of that point from the center."
  []
  (spiral 265149))

(defn two
  "Function to use the different spiral formation - where we create the
  sequence of coordinates for the spiral, and then assign values to each
  of the coordinates, in turn, so that we can locate the points to sum
  in order to add that to the next position."
  [& [ep]]
  (let [tgt (or ep 265149)
        side (fn [d e]
               (let [raw (concat [[d d]]
                           (map (fn [i] [d (- d i)]) (range 1 e))
                           (map (fn [i] [(- d i) (* -1 d)]) (range 1 e))
                           (map (fn [i] [(* -1 d) (- i d)]) (range 1 e))
                           (map (fn [i] [(- i d) d]) (range 1 (dec e))))]
                (concat (rest raw) [(first raw)])))
        src (->> (range)
              (map (fn [i] [i (inc (* 2 i))]))
              (map #(apply side %))
              (apply concat))]
    (loop [coords (rest src)
           pts [{:pos [0 0] :val 1}]]
      (let [nc (first coords)
            nbs (->> (filter #(near? nc (:pos %)) pts)
                  (map :val)
                  (sum))]
        (if (< nbs tgt)
          (recur (rest coords) (conj pts {:pos nc :val nbs}))
          (conj pts {:pos nc :val nbs}))))))
