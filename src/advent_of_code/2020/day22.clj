(ns advent-of-code.2020.day22
  "Twenty-second day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the ingredients and allergens that we need to be
  careful about."
  {:player1 [26 14 6 34 37 9 17 39 4 5 1 8 49 16 18 47 20 31 23 19 35 41 28 15 44]
   :player2 [7 2 10 25 29 46 40 45 11 50 42 24 38 13 36 22 33 3 43 21 48 30 32 12 27]})

(def test1
  "Test data for the first part."
  {:player1 [9 2 6 3 1]
   :player2 [5 8 4 7 10]})

(defn turn
  "Function to take one turn from the game of the two players. This will result
  in the two hands being changed, based on the rules of the game, and is suitable
  for being called over and over to 'play' the game."
  [{p1 :player1 p2 :player2}]
  (let [c1 (first p1)
        c2 (first p2)]
    (if (< c1 c2)
      {:player1 (rest p1) :player2 (concat (rest p2) [c2 c1])}
      {:player1 (concat (rest p1) [c1 c2]) :player2 (rest p2)})))

(defn one
  "Function to play the game, and calculate the score of the winner's hand."
  [& [coll]]
  (let [game (loop [hnds puzzle]
               (if (or (= 0 (count (:player1 hnds))) (= 0 (count (:player2 hnds))))
                 hnds
                 (recur (turn hnds))))
        wins (apply concat (vals game))]
    (->> (map vector (reverse wins) (range 1 (inc (count wins))))
         (map #(apply * %))
         (sum))))

(defn play
  "Function to play a Recurzive Combat game on the input hands, and continue
  to do so, recursively, and to the final conclusion. This includes the infinite
  loop stopper, and playing all the way down."
  [{p1 :player1 p2 :player2 :as arg} past]
  (cond
    (or (= 0 (count p1)) (= 0 (count p2)))
      arg
    (past arg)
      {:player1 (apply concat (vals arg)) :player2 []}
    :else
      (let [c1 (first p1)
            c2 (first p2)]
        (recur
          (cond
            (and (<= c1 (count (rest p1))) (<= c2 (count (rest p2))))
              (let [sg (play {:player1 (take c1 (rest p1)) :player2 (take c2 (rest p2))} #{})]
                (if (not-empty (:player1 sg))
                  {:player1 (concat (rest p1) [c1 c2]) :player2 (rest p2)}
                  {:player1 (rest p1) :player2 (concat (rest p2) [c2 c1])}))
            (< c1 c2)
              {:player1 (rest p1) :player2 (concat (rest p2) [c2 c1])}
            :else
              {:player1 (concat (rest p1) [c1 c2]) :player2 (rest p2)})
          (conj past arg)))))

(defn two
  "Function to play the Recursive Combat game with the new rules for the
  same input."
  [& [coll]]
  (let [game (play puzzle #{})
        wins (apply concat (vals game))]
    (->> (map vector (reverse wins) (range 1 (inc (count wins))))
         (map #(apply * %))
         (sum))))
