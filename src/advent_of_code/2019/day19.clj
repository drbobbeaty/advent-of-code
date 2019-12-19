(ns advent-of-code.2019.day19
  "Nineteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program for the tractor beam"
  (-> (slurp "resources/2019/input/day19.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn render
  "Function to render the output of the program, which is the tractor beam's
  effected area right outside the emitter."
  [tiles]
  (let [bts (keys tiles)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))
        cols (if (empty? bts) 0 (inc (apply max (map first bts))))]
    (for [r (range rows)]
      (->> (for [c (range cols)] (if (pos? (get tiles [c r] 0)) "#" "."))
           (apply str)))))

(defn one
  "Function to look at the 50x50 grid region around the tractor beam, and
  count how many of the grids are effected by the beam. We can also use this
  to visualize the beam, but that's another question. :)"
  [& [mem]]
  (let [ini (if (map? mem) mem {:memory (or mem puzzle)})]
    (->> (for [x (range 50)
               y (range 50)
               :let [tb (first (:output (run (assoc ini :input [x y]))))]
               :when (pos? tb)]
           [[x y] tb])
         (count))))

(defn- santa?
  "Predicate function to look at the beam data and see if a 100x100 grid
  ship would be completely covered by the beam. There is a little fiddling
  with the math because the coordinate checks, but it's pretty clear."
  [beam]
  (if (<= 100 (:y (last beam)))
    (let [bst (filter #(<= 99 (:width %)) (take-last 100 beam))]
      (if (= 100 (count bst))
        (let [xm (:x (last bst))]
          (every? #(<= (+ xm 99) (+ (:x %) (:width %))) bst))))))

(defn two
  "Function to use the Intcode computer code to control the tractor beam
  and see what is the closest point to the emitter when we can safely fit
  a 100x100 grid ship for Santa in the beam. We don't cover the entire
  grid - only the expanding cone of the tractor beam. This cuts down a lot
  on the computing required."
  [& [mem]]
  (let [ini (if (map? mem) mem {:memory (or mem puzzle)})
        tb? (fn [x y] (pos? (first (:output (run (assoc ini :input [x y]))))))
        beam (atom (vec (for [y (range 50)
                              :let [xs (for [x (range 50) :when (tb? x y)] x)]
                              :when (not-empty xs)]
                          {:x (apply min xs) :y y :width (dec (count xs))})))]
    (loop [{xm :x y :y w :width} (last @beam)]
      (let [ny (inc y)
            xs (for [x (range (- xm 2) (+ xm w 4)) :when (tb? x ny)] x)
            nxt {:x (apply min xs) :y ny :width (dec (count xs))}]
        (swap! beam conj nxt)
        (if-not (santa? @beam)
          (recur nxt)
          (let [soln (take-last 100 @beam)
                top (first soln)
                bot (last soln)]
            [[(:x bot) (:y top)] top bot]))))))
