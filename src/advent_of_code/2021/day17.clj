(ns advent-of-code.2021.day17
  "Seventeenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the target for the probe to make it into the trench."
  {:x [124 174] :y [-123 -86]})

(def test1
  "Test data for the first part."
  {:x [20 30] :y [-10 -5]})

(defn step
  "Function to take a position and velocity and compute the next step in
  the trajectory."
  [{x :x y :y vx :vx vy :vy}]
  {:x (+ x vx)
   :y (+ y vy)
   :vx (cond
         (< 0 vx)   (dec vx)
         (zero? vx) vx
         :else      (inc vx))
   :vy (dec vy)})

(defn short?
  "Predicate function to take a tracectory point, and a target, and see if
  we have completely missed the target on the short side, and are no going
  to be able to recover."
  [{x :x y :y vx :vx vy :vy} {[xl xh] :x [yl yh] :y}]
  (and (<= x xh) (zero? vx) (neg? vy) (< y yl)))

(defn hit?
  "Predicate function to take a trajectory point, and a target, and see if
  it's a hit."
  [{x :x y :y} {[xl xh] :x [yl yh] :y}]
  (and (<= xl x xh) (<= yl y yh)))

(defn far?
  "Predicate function to take a tracectory point, and a target, and see if
  we have completely missed the target on the far side, and are no going to
  be able to recover."
  [{x :x y :y vx :vx vy :vy} {[xl xh] :x [yl yh] :y}]
  (and (< xh x) (neg? vy) (< y yl)))

(defn fire
  "Function to fire the probe with the initial velocity towards the target,
  and then either return the trajectory that resulted in a hit, or it fell
  ':short', or ':far', if the probe clearly missed the target."
  [[vx vy] target]
  (loop [traj [{:x 0 :y 0 :vx vx :vy vy}]]
    (let [np (step (last traj))]
      (cond
        (hit? np target)   (conj traj np)
        (far? np target)   :far
        (short? np target) :short
        :else              (recur (conj traj np))))))

(defn apogee
  "Function to compute the apogee of the trajectory - that is the highest
  y value that it achieves in it's flight."
  [t]
  (if (and (not (keyword t)) (not-empty t))
    (apply max (map :y t))
    -1000))

(defn one
  "Function to find the highest apogee of the probe that will hit the
  target at the end of a step, given the target size and location. This
  is not possible to do a ballistic location because it's not the path,
  but the stopping point on each step. So it has to be an exhaustive
  search of reasonable values - based on the location of the target."
  [& [coll]]
  (let [tgt puzzle
        hgt (atom 0)]
    (doseq [vx (range 1 (first (:x tgt)))
            vy (range 1 (first (:x tgt)))
            :let [tf (fire [vx vy] tgt)]]
      (swap! hgt max (apogee tf)))
    @hgt))

(defn two
  "Function to find all paths that will pand the probe in the target
  based on it being there at the end of a step. This is just another
  exhaustive search, but bigger because you can imagine 'jumping'
  right there."
  [& [coll]]
  (let [tgt puzzle
        cnt (atom 0)]
    (doseq [vx (range 1 (inc (last (:x tgt))))
            vy (range (first (:y tgt)) (inc (last (:x tgt))))
            :let [tf (fire [vx vy] tgt)]
            :when (not (keyword? tf))]
      (swap! cnt inc))
    @cnt))
