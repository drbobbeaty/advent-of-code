(ns advent-of-code.2018.day09
  "Ninth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs])
  (import [java.util ArrayList]))

(def puzzle
  "This is the starting point for the puzzle:
    '418 players; last marble is worth 70769 points'"
  {:players 418 :last 70769})

(defn gen
  "Function to generate the next 'play' in the game based on the current state
  of the marble ring. This will include point attribution, but won't say who
  gets the points as the number of players doesn't matter."
  [{lst :last curr :curr ring :ring :as arg}]
  (let [sz (.size ring)
        nm (inc lst)]
    (if (= 0 (mod nm 23))
      (let [pi (if (< curr 7) (+ sz (- curr 7)) (- curr 7))
            ym (.get ring (int pi))]
        (.remove ring ym)
        {:points [nm ym]
         :last   nm
         :curr   pi
         :ring   ring})
      (let [np (if (= (inc curr) sz) 1 (+ 2 curr))]
        (if (= np sz) (.add ring nm) (.add ring np nm))
        {:last nm
         :curr np
         :ring ring}))))

(defn high-score
  "Function to take the number of players and the last marble number, and
  return the highest score - by elf player number, and points."
  [players lmarble]
  (let [bump (fn [m k x] (if (get m k) (update m k + x) (assoc m k x)))
        scores (atom {})]
    (loop [brd (gen {:last 0 :curr 0 :ring (ArrayList. [0])})
           ply 1
           turn 1]
      (when (<= turn lmarble)
        (if-let [pts (:points brd)]
          (swap! scores bump ply (apply + pts)))
        (recur (gen brd) (mod (inc ply) players) (inc turn))))
    (first (sort-by second > @scores))))

(defn one
  "Function to play the game with the elves and to the duration that the
  puzzle required. The point is to find the high score, and that's what
  this function returns - the player's number, and their score."
  []
  (high-score (:players puzzle) (:last puzzle)))

(defn two
  "Function to play the game with the elves and to the duration that the
  puzzle required. The point is to find the high score, and that's what
  this function returns - the player's number, and their score. This just
  happens to have 100* the last marble number."
  []
  (high-score (:players puzzle) (* 100 (:last puzzle))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src [[9 25] [10 1618] [13 7999] [17 1104] [21 6111] [30 5807]]]
    (for [[p lm] src]
      {:players p :last lm :high-score (high-score p lm)})))
