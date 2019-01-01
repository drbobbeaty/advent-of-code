(ns advent-of-code.2016.day19
  "Nineteenth day's solutions for the Advent of Code 2016"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs])
  (:import [java.util ArrayList LinkedList]))

(defn steal
  "Function to play one round of the game for all the remaining elves in the
  game. This will take a sequence of elves in the game, and return a sequence
  of those remaining after one complete 'circle' of the game."
  [s]
  (let [ps (map first (partition 2 s))]
    (if (odd? (count s))
      (concat [(last s)] ps)
      ps)))

(defn cross
  "Function to do a complete processing of all present exchanges for the elf
  sequence provided. This returns the one guy that gets all the goodies."
  [s]
  (let [ply (ArrayList. s)]
    (loop [pos 0]
      (let [cnt (.size ply)
            idx (mod (+ pos (quot cnt 2)) cnt)
            nxt (.remove ply (int idx))]
        (prn pos idx nxt ply)
        ; (if (= 0 (mod cnt 10000))
        ;   (prn cnt (quot (System/currentTimeMillis) 1000)))
        (if (< 2 cnt)
          (recur (mod (inc pos) cnt))
          {:elf (first ply)})))))

(defn one
  "Function to sew which elf gets to keep all the presents in the game."
  []
  (loop [grp (range 1 (inc 3014387))]
    (let [nxt (steal grp)]
      (if (< 1 (count nxt))
        (recur nxt)
        nxt))))

; (defn two
;   "Function to see which elf gets all the presents on the 'take across' rules,
;   and prints him out."
;   []
;   (cross (range 1 (inc 3014387))))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (loop [s [1 2 3 4 5]]
    (let [nxt (steal s)]
      (if (< 1 (count nxt))
        (recur nxt)
        nxt))))
