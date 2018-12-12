(ns advent-of-code.2018.day12
  "Twelveth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]))

(defn gen
  "Function to take an `alive` function and a string representing the plants in
  the pots in the cave, and generates the next generation of the plants in the
  pots and returns that."
  [afn s]
  (->> (partition 5 1 (concat [\. \.] s [\. \.]))
    (map #(if (afn %) \# \.))))

(defn grow
  "Function to run some number of generations on the plants, given a starting
  configuration, and the rules for when a new plat pops up, and then sum the
  numbers on the pots that have plants in them at that final generation."
  [start keeps nums]
  (let [sz (count start)
        buff (apply str (repeat sz "."))
        live? (set (map seq keeps))
        off (atom 0)
        edge (fn [s] (if (some #(= \# %) (concat (take 5 s) (take-last 5 s)))
                       (do
                         (swap! off + sz)
                         (concat (seq buff) s (seq buff)))
                       s))
        fin (loop [pots start
                   cnt nums]
              (if (pos? cnt)
                (recur (gen live? (edge pots)) (dec cnt))
                pots))]
    (->> fin
      (map vector (range (- @off) Long/MAX_VALUE))
      (filter #(= \# (second %)))
      (map first)
      (apply +))))

(defn one
  "Function to run 20 generations on the plants, and then sum the numbers on
  the pots that have plants in them at that time."
  []
  (grow "#.####...##..#....#####.##.......##.#..###.#####.###.##.###.###.#...#...##.#.##.#...#..#.##..##.#.##"
        ["..##." ".#..#" "..#.." "###.." "##..." "#.###" ".##.#"
         ".#..." "...#." ".#.##" "..#.#" "#..#." ".###." "##.##"]
        200))

(defn two
  "Function to run 50 bil generations on the plants, and then sum the numbers
  on the pots that have plants in them at that time."
  []
  (let [go 160
        pco 13542]
    (+ pco (* 72 (- 50000000000 160)))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (grow "#..#.#..##......###...###"
        ["...##" "..#.." ".#..." ".#.#." ".#.##" ".##.." ".####"
         "#.#.#" "#.###" "##.#." "##.##" "###.." "###.#" "####."]
        20))

(defn bobo
  "Function to run thorough a series of generations, once we know we are
  in the 'stable, growing' reagion of the evolution, and so the expansion
  formula holds."
  []
  (let [start "#.####...##..#....#####.##.......##.#..###.#####.###.##.###.###.#...#...##.#.##.#...#..#.##..##.#.##"
        keeps ["..##." ".#..#" "..#.." "###.." "##..." "#.###" ".##.#"
               ".#..." "...#." ".#.##" "..#.#" "#..#." ".###." "##.##"]]
    (for [c (range 160 200)]
      [c (grow start keeps c)])))
