(ns advent-of-code.2019.day22
  "Twenty-second day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the shuffle operations to perform on a new deck."
  (-> (slurp "resources/2019/input/day22.txt")
      (cs/trim)
      (cs/split #"\n")))

(def trial1
  "Test data for part 1 - final order of small deck: (0 3 6 9 2 5 8 1 4 7)"
  ["deal with increment 7"
   "deal into new stack"
   "deal into new stack"])

(def trial2
  "Test data for part 1 - final order of small deck: (3 0 7 4 1 8 5 2 9 6)"
  ["cut 6"
   "deal with increment 7"
   "deal into new stack"])

(def trial3
  "Test data for part 1 - final order of small deck: (6 3 0 7 4 1 8 5 2 9)"
  ["deal with increment 7"
   "deal with increment 9"
   "cut -2"])

(def trial4
  "Test data for part 1 - final order of small deck: (9 2 5 8 1 4 7 0 3 6)"
  ["deal into new stack"
   "cut -2"
   "deal with increment 7"
   "cut 8"
   "cut -4"
   "deal with increment 7"
   "cut 3"
   "deal with increment 9"
   "deal with increment 3"
   "cut -1"])

(defn new-stack
  "Function to implement the 'new-stack' operation, which is pretty simple."
  [s]
  (reverse s))

(defn cut-n-cards
  "Function to implement the 'cut cards' operation - both positive and negative."
  [s n]
  (if (pos? n)
    (concat (drop n s) (take n s))
    (concat (take-last (Math/abs n) s) (drop-last (Math/abs n) s))))

(defn deal
  "Function to implement the 'deal n' operation which is a little interesting,
  in that we use an atom to make it easy to place the cards in the right order,
  but that's still not too bad."
  [s n]
  (let [cnt (count s)
        ans (transient (vec (repeat cnt 0)))]
    (doseq [[c i] (map vector s (range))
            :let [idx (mod (* n i) cnt)]]
      (assoc! ans idx c))
    (persistent! ans)))

(defn dealer
  "Function to take a series of shuffle operations, shs, and a deck of cards,
  and run all the operations, in order, on the deck - returning the output of
  the deck to the caller."
  [shs deck]
  (loop [src shs
         dk deck]
    (if-let [l (first src)]
      (cond
        (= "deal into new stack" l)
          (recur (rest src) (new-stack dk))
        (.startsWith l "cut ")
          (let [a (parse-int (last (re-matches #"cut (.*)" l)))]
            (recur (rest src) (cut-n-cards dk a)))
        (.startsWith l "deal with increment ")
          (let [a (parse-int (last (re-matches #"deal with increment (.*)" l)))]
            (recur (rest src) (deal dk a))))
      dk)))

(defn one
  "Function to run the puzzle input on the factory new deck of 10007 cards, and
  find the location of the card with '2019' on it."
  []
  (.indexOf (dealer puzzle (range 10007)) 2019))

(defn rdealer
  ""
  [shs sz fpos]
  (loop [src (reverse shs)
         pos fpos]
    (if-let [l (first src)]
      (cond
        (= "deal into new stack" l)
          (recur (rest src) (- sz pos 1))
        (.startsWith l "cut ")
          (let [a (parse-int (last (re-matches #"cut (.*)" l)))
                aa (Math/abs a)
                os (- sz aa)]
            (if (pos? a)
              (if (<= os pos)
                (recur (rest src) (- pos os))
                (recur (rest src) (+ pos aa)))
              (if (< pos aa)
                (recur (rest src) (+ pos os))
                (recur (rest src) (- pos aa)))))
        (.startsWith l "deal with increment ")
          (let [a (parse-int (last (re-matches #"deal with increment (.*)" l)))
               ]
            (if (zero? pos)
              (recur (rest src) 0)
              (let [idx (first
                          (for [i (range sz)
                                :when (= pos (mod (* a i) sz))]
                            i))
                   ]
                (recur (rest src) idx)
              )
            )))
      pos)))

(defn bobo
  ""
  []
  (map #(rdealer trial4 10 %) (range 10))
  ; (dealer ["deal with increment 7"] (range 10))
  )

(defn yoyo
  ""
  []
  (loop [pos 2020
         cnt 0
         ans (transient [[0 2020]])]
    (let [ppos (rdealer puzzle 119315717514047 pos)]
      (infof "cnt: %d ... ppos: %d" (inc cnt) ppos)
      (if (and (< cnt 20) (not= ppos 2020))
        (recur ppos (inc cnt) (conj! ans [(inc cnt) ppos]))
        (persistent! ans)
      )
    )
  )
  )
