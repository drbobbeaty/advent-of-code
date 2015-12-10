(ns advent-of-code.day10
  (:require [clojure.string :as cs]))

(defn say
  "Function to do 'look-and-say' for one incoming number - as a string."
  [s]
  (if (string? s)
    (apply str (for [p (partition-by identity s)]
                 (str (count p) (first p))))))

(defn crank
  "Function to loop through the `say` function `n` times."
  [n s]
  (loop [cnt 0
         las s]
    (if (= cnt n)
      las
      (recur (inc cnt) (say las)))))