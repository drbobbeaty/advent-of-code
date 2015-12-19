(ns advent-of-code.day18
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the initial light configuration."
  (apply concat (for [l (cs/split-lines (slurp "resources/input/day18.txt"))]
                  (map #(= \# %) l))))

(defn next-gen
  ""
  [w nr nc]
  (for [r (range nr)
        c (range nc)
        :let [me (nth w (+ c (* r nc)))
              tc (if (< 0 c (dec nc)) 3 2)
              fdc (+ (max 0 (dec c)) (* (dec r) nc))
              pr (if (pos? r) (take tc (drop fdc w)))
              mr (take tc (drop (+ fdc nc) w))
              nr (if (< r nr) (take tc (drop (+ fdc nc nc) w)))
              cnt (count (filter identity (concat pr mr nr)))]]
    (if me (<= 3 cnt 4) (= 3 cnt))))

(defn hit-corners
  ""
  [w nr nc]
  (for [r (range nr)
        c (range nc)
        :let [me (nth w (+ c (* r nc)))]]
    (if (or (and (= 0 c) (= 0 r))
            (and (= (dec nc) c) (= 0 r))
            (and (= 0 c) (= (dec nr) r))
            (and (= (dec nc) c) (= (dec nr) r)))
      true
      me)))

(defn part1
  ""
  []
  (let [nr 100
        nc 100
        nit 100]
    (->> (loop [bd puzzle
                itr 0]
           (if (= nit itr)
             bd
             (recur (next-gen bd nr nc) (inc itr))))
         (filter identity)
         (count))))

(defn part2
  ""
  []
  (let [nr 100
        nc 100
        nit 100]
    (->> (loop [bd puzzle
                itr 0]
           (if (= nit itr)
             bd
             (recur (hit-corners (next-gen (hit-corners bd nr nc) nr nc) nr nc) (inc itr))))
         (filter identity)
         (count))))
