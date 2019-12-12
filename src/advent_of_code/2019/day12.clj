(ns advent-of-code.2019.day12
  "Twelfth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum lcm]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the observed locations of the four Jupiter moons."
  [{:x -7 :y -8 :z 9}
   {:x -12 :y -3 :z -4}
   {:x 6 :y -17 :z -9}
   {:x 4 :y -10 :z -6}])

(def trial1
  "This is test data for part 1 - total energy after 10 steps = 179"
  [{:x -1 :y 0 :z 2}
   {:x 2 :y -10 :z -7}
   {:x 4 :y -8 :z 8}
   {:x 3 :y 5 :z -1}])

(def trial2
  "This is test data for part 1 - total energy after 100 steps = 1940"
  [{:x -8 :y -10 :z 0}
   {:x 5 :y 5 :z 10}
   {:x 2 :y -7 :z 3}
   {:x 9 :y -8 :z -3}])

(defn step
  "Function to take a series of moon positions, with velocities, and compute
  the new velocities, and then the new positions based on those updated
  velocities. The output will be the time-based evolution of just one step
  in time."
  [coll]
  (let [acc (fn [a b] (cond (< a b) 1 (= a b) 0 :else -1))
        vadd (fn [a b] (map sum (map vector a b)))
        newv (doall (for [a coll
                  :let [[ax ay az] (->> (for [b (remove #(= a %) coll)]
                                          [(acc (:x a) (:x b)) (acc (:y a) (:y b)) (acc (:z a) (:z b))])
                                        (apply map vector)
                                        (map sum))]]
               (assoc a :vx (+ ax (or (:vx a) 0)) :vy (+ ay (or (:vy a) 0)) :vz (+ az (or (:vz a) 0)))))]
    (doall (for [a newv]
      (assoc a :x (+ (:x a) (:vx a)) :y (+ (:y a) (:vy a)) :z (+ (:z a) (:vz a)))))))

(defn energy
  "Function to compute the total energy of the moons - potential and kinetic,
  and then returns the product - as that's what the puzzle wanted."
  [m]
  (let [pe (sum (map abs (map m [:x :y :z])))
        ke (sum (map abs (map m [:vx :vy :vz])))]
    (* pe ke)))

(defn one
  "Function to return the sum of the total energy of all the moons after
  1000 steps of movement."
  [& [coll]]
  (sum (map energy (nth (iterate step (or coll puzzle)) 1000))))

(defn- period
  "Function to return the period, in steps, for the key value in the
  collection of moon positions (:x, :y, or :z) for the stepping to return
  to the same values as the original set. This is meant to find the period
  of the repeating of *all* values, but this is achieved by independently
  looking at each axis."
  [coll k]
  (let [tgt (map k coll)]
    (loop [loc (step coll)
           stps 1]
      (if (= tgt (map k loc))
        stps
        (if (< stps 1000000)
          (recur (step loc) (inc stps))
          :failed)))))

(defn two
  "Function to find the number of steps it'll take to repeat the position
  and velocity of the initial starting set. This means finding the period
  of each of the axis, and then finding the LCM of them to get the period
  of the combined set. The 'inc' in the period is for the velocity at the
  turn-around - and we need it to match as well."
  [& [coll]]
  (let [src (or coll puzzle)]
    (->> (map #(period src %) [:x :y :z])
         (map inc)
         (apply lcm))))
