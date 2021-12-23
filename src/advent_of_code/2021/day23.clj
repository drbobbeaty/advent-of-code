(ns advent-of-code.2021.day23
  "Twenty-third day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn one
  "Function to compute the cost of the manual solution to the puzzle.

     #############
     #...........#
     ###D#D#A#A###
       #C#C#B#B#
       #########

     #############
     #.A.......B.#  8A 3B
     ###D#D#A#.###
       #C#C#B#.#
       #########

     #############
     #.A.......B.#  8A 3B ... 15D
     ###.#.#A#D###
       #C#C#B#D#
       #########

     #############
     #.A.B...A.B.#  8A 3B ... 15D ... 2A 5B
     ###.#.#.#D###
       #C#C#.#D#
       #########

     #############
     #.A.....A.B.#  8A 3B ... 15D ... 2A 5B
     ###.#.#.#D###  6C 3B
       #C#B#C#D#
       #########

     #############
     #.A.....A.B.#  8A 3B ... 15D ... 2A 5B
     ###.#.#C#D###  6C 3B ... 7C
       #.#B#C#D#
       #########

     #############
     #.A.....A.B.#  8A 3B ... 15D ... 2A 5B
     ###.#.#C#D###  6C 3B ... 7C ... 9A 6B
       #.#B#C#D#
       #########    = 19A + 17B + 13C + 15D"
  [& [coll]]
  (+ 19 (* 10 17) (* 100 13) (* 1000 15)))

(defn two
  "Function to compute the cost of the manual solution to the puzzle.

    #############
    #...........#
    ###D#D#A#A###
      #D#C#B#A#
      #D#B#A#C#
      #C#C#B#B#
      #########

    #############
    #AA.......BC#  18A 5C 5B
    ###D#D#A#.###
      #D#C#B#.#
      #D#B#A#.#
      #C#C#B#.#
      #########

    #############
    #AA.......BC#  18A 5C 5B ... 39D
    ###.#.#A#D###
      #.#C#B#D#
      #.#B#A#D#
      #C#C#B#D#
      #########

    #############
    #AA.....C.BC#  18A 5C 5B ... 39D
    ###.#.#A#D###  9C
      #.#C#B#D#
      #.#B#A#D#
      #.#C#B#D#
      #########

    #############
    #.B.....C.BC#  18A 5C 5B ... 39D
    ###.#.#.#D###  9C ... 17A 7B
      #A#C#.#D#
      #A#B#A#D#
      #A#C#B#D#
      #########

    #############
    #.B.B...C.BC#  18A 5C 5B ... 39D
    ###A#.#.#D###  9C ... 17A 7B ... 8A 7B
      #A#C#.#D#
      #A#B#.#D#
      #A#C#.#D#
      #########

    #############
    #.B.B...B.BC#  18A 5C 5B ... 39D
    ###A#.#.#D###  9C ... 17A 7B ... 8A 7B
      #A#.#.#D#    12C 6B
      #A#.#C#D#
      #A#C#C#D#
      #########

    #############
    #.B.....B.BC#  18A 5C 5B ... 39D
    ###A#.#.#D###  9C ... 17A 7B ... 8A 7B
      #A#.#C#D#    12C 6B ... 8C 22B 5C
      #A#.#C#D#
      #A#B#C#D#    = 18+17+8A 5+7+7+6+22B 5+9+8+12+5C 39D
      #########    = 43A + 47B + 39C + 39D"
  [& [coll]]
  (+ 43 (* 10 47) (* 100 39) (* 1000 39)))
