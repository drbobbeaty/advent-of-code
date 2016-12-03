(ns advent-of-code.2015.day21
  (:require [clojure.string :as cs]))

(def boss
  {:hit-points 100
   :damage 8
   :armor 2})

(defn play
  "Function to play a battle - taking turns to see who wins the battle - the
  boss or me."
  [pl bo]
  (let [ps (max 1 (- (:damage pl) (:armor bo)))
        bs (max 1 (- (:damage bo) (:armor pl)))]
    (loop [pi pl
           bi bo]
      (let [bhp (- (:hit-points bi) ps)
            php (- (:hit-points pi) bs)]
        (cond
          (<= bhp 0)
            "Player wins"
          (<= php 0)
            "Boss wins"
          (and (pos? bhp) (pos? php))
            (recur (assoc pi :hit-points php) (assoc bi :hit-points bhp))
          :else
            "Tie?")))))

(defn solo
  ""
  [d a]
  (let [me {:hit-points 100
            :damage d
            :armor a}]
    (play me boss)))

(defn damage-cost
  "Table of all the costs of each level of damage - based on the function
  applied to a sequence of all the possible costs of acquiring that level."
  [f pts]
  (case pts
    4 8
    5 (f 10 (+ 8 25))
    6 (f 25 (+ 10 25) (+ 8 50))
    7 (f 40 (+ 25 25) (+ 10 50) (+ 8 100) (+ 8 50 25))
    8 (f 74 (+ 40 25) (+ 25 50) (+ 10 100) (+ 10 50 25))
    9 (f (+ 74 25) (+ 40 50) (+ 25 100) (+ 25 50 25) (+ 10 100 25) (+ 8 100 50))
    10 (f (+ 74 50) (+ 40 100) (+ 40 50 25) (+ 25 100 25) (+ 10 100 50))
    11 (f (+ 74 100) (+ 40 100 25) (+ 25 100 50))
    0))

(defn armor-cost
  "Table of all the costs of each level of armor - based on the function
  applied to a sequence of all the possible costs of acquiring that level."
  [f pts]
  (case pts
    1 13
    2 (f 31 (+ 13 20))
    3 (f 53 (+ 31 20) (+ 13 40))
    4 (f 75 (+ 53 20) (+ 31 40) (+ 13 80))
    5 (f 102 (+ 75 20) (+ 53 40) (+ 31 80))
    6 (f (+ 102 20) (+ 75 40) (+ 53 80))
    7 (f (+ 102 40) (+ 75 80))
    8 (+ 102 80)
    0))

(defn price-game
  "Find the winners, and sort them by cost. Simple."
  [win pf]
  (sort-by :cost (for [d [4 5 6 7 8 9 10 11]
                       a [0 1 2 3 4 5 6 7 8]
                       :let [me {:hit-points 100
                                 :damage d
                                 :armor a}
                             gm (play me boss)]
                       :when (= win gm)]
                   { :damage d
                     :armor a
                     :game gm
                     :cost (+ (damage-cost pf d) (armor-cost pf a)) })))

(defn just-needs
  "Find the winners, and sort them by cost. Simple."
  []
  (price-game "Player wins" min))

(defn most-loose
  "Find the losers, and sort them by cost. Simple."
  []
  (price-game "Boss wins" max))
