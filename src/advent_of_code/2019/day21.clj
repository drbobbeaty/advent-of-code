(ns advent-of-code.2019.day21
  "Twenty first day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum is-ascii?]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program - ASCII"
  (-> (slurp "resources/2019/input/day21.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn image
  "Function to extract the image data from the data stream coming from the
  camera. This puts the data into a format we can use, and render nicely."
  [data]
  (loop [src data
         row 0
         col 0
         tiles (transient {})]
    (if-let [px (first src)]
      (cond
        (= 10 px)
          (recur (rest src) (inc row) 0 tiles)
        (is-ascii? px)
          (recur (rest src) row (inc col) (assoc! tiles [col row] (char px)))
        :else
          (do
            (infof "invalid character value in Intcode output: %d" px)
            (recur (rest src) row (inc col) tiles)))
      (persistent! tiles))))

(defn render
  "Function to render the output of the camera, as defined by the output of
  the Intcode program - ASCII."
  [tiles]
  (let [bts (keys tiles)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))]
    (for [r (range rows)
          :let [rd (for [[k v] tiles :when (= r (second k))] [k v])]]
        (->> (sort-by (comp first first) rd)
             (map second)
             (apply str)))))

(defn spring
  "Function to run the springbot program supplied with the Intcode program
  optionally supplied - we assume the puzzle input in this case. The reason
  is that the challenge is the springbot program and not the Intcode program.
  This will render the output as well as log the CPU state - minus the memory
  and output - just to see what's happened."
  [pgm & [mem]]
  (let [ini (if (map? mem) mem {:memory (or mem puzzle)})
        inp (map int (str (cs/join "\n" pgm) "\n"))
        cpu (run (assoc ini :input inp))]
    (infof "cpu: %s" (pr-str (dissoc cpu :memory :output)))
    (render (image (:output cpu)))))

(defn one
  "Function to program the springbot to jump over any reasonable hole in
  front of it. The logic is: D && (!A || !B || !C) -- implemented in the
  springbot assembly. The answer is in the logs, as an unprintable ASCII
  value."
  []
  (spring ["NOT A J"
           "NOT J T"
           "AND B T"
           "AND C T"
           "NOT T J"
           "AND D J"
           "WALK"]))

(defn two
  "Function to 'run' the springbot all over the ship with the long-range
  sensors, and not fall in. The logic was just to add (E || H) to the
  previous logic, and it worked like a charm."
  []
  (spring ["NOT A J"
           "NOT J T"
           "AND B T"
           "AND C T"
           "NOT T J"
           "AND D J"
           "NOT E T"
           "NOT T T"
           "OR H T"
           "AND T J"
           "RUN"]))
