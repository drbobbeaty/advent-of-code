(ns advent-of-code.2021.day21
  "Twenty-first day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum hr-in-millis]]
            [clojure.core.memoize :as memo]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the starting player positions."
  {:player1 7 :player2 8})

(def test1
  "Test data for the first part."
  {:player1 4 :player2 8})

(defn one
  "Function to find the final product of loser score and dice rolls for the
  test game where the players start given the puzzle."
  [& [coll]]
  (loop [ai (:player1 puzzle)
         asc 0
         bi (:player2 puzzle)
         bsc 0
         die (map #(inc (mod % 100)) (range))
         cnt 0]
    (let [aroll (sum (take 3 die))
          broll (sum (take 3 (drop 3 die)))
          nai (inc (mod (dec (+ ai aroll)) 10))
          nbi (inc (mod (dec (+ bi broll)) 10))]
      (cond
        (<= 1000 (+ asc nai))
          {:ai nai :ascore (+ asc nai) :bi bi :bscore bsc :rolls (+ cnt 3) :final (* bsc (+ cnt 3))}
        (<= 1000 (+ bsc nbi))
          {:ai nai :ascore (+ asc nai) :bi nbi :bscore (+ bsc nbi) :rolls (+ cnt 6) :final (* (+ asc nai) (+ cnt 6))}
        :else
          (recur nai (+ asc nai) nbi (+ bsc nbi) (drop 6 die) (+ 6 cnt))))))

(declare play)

(defn play*
  "Function to play a game from the point of two players, a & b, and their
  positions, ai & bi, and their scores, asc & bsc. This will play each
  universe to the end, and then return with the two scores of wins for
  each player."
  [ai asc bi bsc]
  (if (<= 21 bsc)
    [0 1]
    (reduce
      (fn [wins d1]
        (reduce
          (fn [wins d2]
            (reduce
              (fn [wins d3]
                (let [go (fn [pos]
                           (->> (play bi bsc pos (+ asc pos 1))
                             (map vector (range))
                             (map (fn [[i w]] (+ w (nth wins (- 1 i)) )))
                             (reverse)))]
                  (go (mod (+ ai d1 d2 d3) 10))))
              wins
              [1 2 3]))
          wins
          [1 2 3]))
      [0 0]
      [1 2 3])))

(def play
  "Memoized unction to play a game from the point of two players, a & b,
  and their positions, ai & bi, and their scores, asc & bsc. This will
  play each universe to the end, and then return with the two scores of
  wins for each player."
  (memo/ttl play* :ttl/threshold hr-in-millis))

(defn two
  "Function to find the number of wins for the player that wins the most
  with the Dirac Dice."
  [& [coll]]
  (apply max (play (dec (:player1 puzzle)) 0 (dec (:player2 puzzle)) 0)))
