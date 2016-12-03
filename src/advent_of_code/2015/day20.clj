(ns advent-of-code.2015.day20
  (:require [clojure.string :as cs]))

(defn presents
  "Compute the number of presents delivered to house `n` by the elves. This
  one has the elves delivering non-stop, so the only check is if they need
  to make a stop to this house."
  [n]
  (apply + (map #(* 10 %) (filter #(zero? (mod n %)) (map inc (range n))))))

;; [786240 34137600]

(defn lazy-presents
  "Compute the number of presents delivered to house `n` by the elves. This
  one has the elves delivering for 50 stops, so we have to add the check that
  we are still in the 'delivery range' of the elve. Also, they deliver one
  more present to account for their lazyness."
  [n]
  (let [keep? (fn [i] (and (zero? (mod n i)) (<= n (* i 50))))]
    (apply + (map #(* 11 %) (filter keep? (map inc (range n)))))))

;; [831600 35780206]

(defn checker
  "Function to find the house with the minimum number of presents. This is a
  search, and we have to start somewhere, but let's make it a reasonable
  number so we don't waste too many CPU cycles too early in the delivery."
  [ptf]
  (loop [c 600000]
    (let [p (ptf c)]
      (if (<= 34000000 p)
        [c p]
        (recur (inc c))))))
