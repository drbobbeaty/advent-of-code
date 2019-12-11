(ns advent-of-code.2019.day11
  "Eleventh day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the Intcode program for the hull painting robot."
  (-> (slurp "resources/2019/input/day11.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn paint
  "Function to take an Intcode program, and an optional first section color,
  and run the robot painting program - returning a set of all the white
  panels that is then suitable for converting into an image with 'render'."
  [& [coll fsc]]
  (let [white (atom (if (= 1 fsc) #{[0 0]} #{}))
        paint (atom #{})]
    (loop [cpu {:memory (or coll puzzle) :input []}
           pos [0 0]
           dir "^"
           stps 0]
      (let [nxt (update cpu :input concat [(if (@white pos) 1 0)])
            ocol (run (assoc nxt :io-wait true))
            sqc (first (:output ocol))
            odir (run (assoc ocol :output [] :io-wait true))
            trn (first (:output odir))
            ndir (case dir
                   "^" (if (= 0 trn) "<" ">")
                   ">" (if (= 0 trn) "^" "v")
                   "v" (if (= 0 trn) ">" "<")
                   "<" (if (= 0 trn) "v" "^"))
            npos (case ndir
                   "^" [(first pos) (dec (second pos))]
                   ">" [(inc (first pos)) (second pos)]
                   "v" [(first pos) (inc (second pos))]
                   "<" [(dec (first pos)) (second pos)])]
        (swap! white (if (= 1 sqc) conj disj) pos)
        (swap! paint conj pos)
        (if (#{:halt :exception} (:state odir))
          {:white @white :paint @paint}
          (recur (assoc odir :output []) npos ndir (inc stps)))))))

(defn one
  "Function to run the painting robot code, and then count the number of
  tiles that the robot would have painted - regardless of color - but not
  counting any tile twice."
  [& [coll]]
  (let [ans (paint (or coll puzzle))]
    (count (:paint ans))))

(defn render
  "Function to take a set of points [x y], and for each point, render a '*'
  on a field of spaces. This will return a sequence of strings - as wide
  and tall as the data extremes of the 'painted' dots."
  [sp]
  (let [rvs (map second sp)
        [minr maxr] (if (empty? rvs) [0 0] [(apply min rvs) (apply max rvs)])
        cvs (map first sp)
        [minc maxc] (if (empty? cvs) [0 0] [(apply min cvs) (apply max cvs)])
        rows (inc (- maxr minr))
        cols (inc (- maxc minc))]
    (for [r (range rows)]
      (apply str (for [c (range cols)] (if (sp [(+ c minc) (+ r minr)]) "*" " "))))))

(defn two
  "Function to start on a white panel and let the robot painting program make
  the output that it needs, and then render that data so that we can read what
  it's supposed to be."
  [& [coll]]
  (let [ans (paint (or coll puzzle) 1)]
    (render (:white ans))))
