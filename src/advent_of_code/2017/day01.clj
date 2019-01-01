(ns advent-of-code.2017.day01
  "First day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [compact sum]]
            [clojure.string :as cs]))

(def puzzle
  "This is the sequence of single-digit numbers from the captcha for getting
  out of the room."
  (->> (slurp "resources/2017/input/day01.txt")
    (cs/trim)
    (mapv #(- (int %) 48))))

(defn one
  "Function to compute the captcha for the input provided. Just a single sum
  of all the doubled digits."
  []
  (let [src puzzle
        dbl (fn [[a b]] (if (= a b) a))]
    (let [prs (partition 2 1 (conj src (first src)))]
      (sum (compact (map dbl prs))))))

(defn two
  "Function to recompute the captcha on the half-ahead scheme."
  []
  (let [src puzzle
        dbl (fn [[a b]] (if (= a b) a))]
    (let [os (/ (count src) 2)
          dt (concat src src)
          prs (for [i (range (count src))] [(nth dt i) (nth dt (+ i os))])]
      (sum (compact (map dbl prs))))))

(defn yoyo
  ""
  []
  (let [src [[1 2 1 2]
             [1 2 2 1]
             [1 2 3 4 2 5]
             [1 2 3 1 2 3]
             [1 2 1 3 1 4 1 5]]
        dbl (fn [[a b]] (if (= a b) a))]
    (for [t src
          :let [os (/ (count t) 2)
                dt (concat t t)
                prs (for [i (range (count t))] [(nth dt i) (nth dt (+ i os))])]]
      (sum (compact (map dbl prs))))))
