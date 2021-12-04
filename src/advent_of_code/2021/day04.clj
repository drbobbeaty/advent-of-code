(ns advent-of-code.2021.day04
  "Fourth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum transpose]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse-brd
  "Function that takes the sequence of 5 strings that is the input for a board,
  and converts it to a sequence of 5 sequences of integers which is a lot
  easier to manipulate for the rest of the puzzle."
  [coll]
  (->> coll
    (map trim)
    (map #(cs/replace % #"  " " "))
    (map #(split % #" "))
    (map #(map parse-int %))))

(defn parse
  "Function to parse the input into the calling sequence of numbers, and the
  sequence of boards - each a sequence of sequences."
  [coll]
  (let [calls (map parse-int (split (first coll) #","))
        brds (map (comp parse-brd rest) (partition 6 (rest coll)))]
    {:calls calls :boards brds}))

(def puzzle
  "This is the input of the bingo call sequence and the boards to play with."
  (-> (slurp "resources/2021/input/day04.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
       ""
       "22 13 17 11  0"
       " 8  2 23  4 24"
       "21  9 14 16  7"
       " 6 10  3 18  5"
       " 1 12 20 15 19"
       ""
       " 3 15  0  2 22"
       " 9 18 13 17  5"
       "19  8  7 25 23"
       "20 11 10 24  4"
       "14 21 16 12  6"
       ""
       "14 21 17 24  4"
       "10 16 15  9 19"
       "18  8 23 26 20"
       "22 11 13  6  5"
       " 2  0 12  3  7"]
    (parse)))

(defn winner?
  "Predicate function to test to see if any of the rows or columns in the
  board are all 'played' - or in this case, have their thousands set. If
  so, then this will return true, else, false."
  [board]
  (let [hit? (fn [b] (some #(every? (fn [n] (<= 1000 n)) %) b))]
    (or (hit? board) (hit? (transpose board)))))

(defn turn
  "Function to take a called number and a board as a sequence of sequences and
  'plays' the called number on the bingo board and sets any matching number to
  'played' by adding 1000 to it (setting the thousands bit) so that it's clear
  what the board number is - but also that it's been played."
  [call board]
  (for [row board]
    (map #(if (= call %) (+ 1000 %) %) row)))

(defn one
  "Function to find the first winning board, based on the call sequence and
  the boards as pased in from the puzzle input. This will compute the score
  as directed in the problem statement - sum of unplayed squares times the
  last called number."
  [& [coll]]
  (loop [calls (:calls puzzle)
         brds (:boards puzzle)]
    (let [call (first calls)
          nxt (map #(turn call %) brds)
          wins (filter winner? nxt)]
      (if-not (empty? wins)
        (let [win (first wins)
              unmk (sum (filter #(< % 1000) (apply concat win)))]
          {:board win :score (* call unmk) :last call :unmarked unmk})
        (recur (rest calls) nxt)))))

(defn two
  "Function to find the LAST winning board in the bingo game, and the changes
  to the first part are minor - the test for 'dnoe' are that there is one
  winner, and only one playing board - and that the looping filters out the
  winners as they are of no interest to me any more."
  [& [coll]]
  (loop [calls (:calls puzzle)
         brds (:boards puzzle)]
    (let [call (first calls)
          nxt (map #(turn call %) brds)
          wins (filter winner? nxt)]
      (if (= 1 (count nxt) (count wins))
        (let [win (first wins)
              unmk (sum (filter #(< % 1000) (apply concat win)))]
          {:board win :score (* call unmk) :last call :unmarked unmk})
        (recur (rest calls) (remove winner? nxt))))))
