(ns advent-of-code.2022.day02
  "Second day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]))

(def puzzle
  "This is the input of the "
  (-> (slurp "resources/2022/input/day02.txt")
    (trim)
    (split #"\n")
    (->> (map (fn [s] [(first s) (last s)])))))

(def test1
  "Test data for the first part."
  (-> ["A Y"
       "B X"
       "C Z"]
    (->> (map (fn [s] [(first s) (last s)])))))

(def rules
  "This is a simple table of the outcomes for the different win/lose/draw
  games where the first character is me, and the second character is him."
  {:XA 3 :XB 0 :XC 6
   :YA 6 :YB 3 :YC 0
   :ZA 0 :ZB 6 :ZC 3})

(defn score
  "Function to take the two players in the RPS game and return the total
  score of the game with the points for my piece and the outcome included."
  [[him me]]
  (let [mp (.indexOf " XYZ" (str me))
        wld ((keyword (str me him)) rules)]
    (+ mp wld)))

(defn one
  "Function to find the total score I'd have in RPS if I followed the
  game plan exactly."
  [& [coll]]
  (->> puzzle
    (map score)
    (sum)))

(def my-pick
  "These are the plays I need to make based on his piece and the desired
  outcome."
  {:AX \Z :AY \X :AZ \Y
   :BX \X :BY \Y :BZ \Z
   :CX \Y :CY \Z :CZ \X})

(defn pick
  "Convert the his-play and outcome to his-play my-play so that we can
  then use score to come up with the score for this round."
  [[him res]]
  [him ((keyword (str him res)) my-pick)])

(defn two
  "Function to find the total score I'd get if I played by the win/lose/draw
  rules in the dataset."
  [& [coll]]
  (->> puzzle
    (map pick)
    (map score)
    (sum)))
