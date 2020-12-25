(ns advent-of-code.2020.day25
  "Twenty-fifth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of "
  {:card 11404017 :door 13768789})

(def test1
  "Test data for the first part."
  {:card 5764801 :door 17807724})

(defn loop-size
  "Function to compute the loop size for a given public key."
  [pkey & [subj]]
  (let [go (fn [n] (mod (* n (or subj 7)) 20201227))]
    (->> (rest (iterate go 1))
         (map vector (rest (range)))
         (filter #(= pkey (last %)))
         (first)
         (first))))

(defn fward
  "Function to play forward the looping on the sbuject value, so that we can
  obtain the encryption key."
  [subj cnt]
  (loop [n 1
         lc 0]
    (if (< lc cnt)
      (recur (mod (* n subj) 20201227) (inc lc))
      n)))

(defn one
  "Function to find the encryption key for the door to the resort room."
  [& [m]]
  (let [inp (or m puzzle)]
    (fward (:card inp) (loop-size (:door inp)))))
