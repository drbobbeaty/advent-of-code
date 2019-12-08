(ns advent-of-code.2019.day08
  "Eighth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int sum]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the image data from the elves."
  (->> (slurp "resources/2019/input/day08.txt")
       (cs/trim)
       (partition 25)
       (partition 6)))

(defn one
  "Function to look at the image data, find the layer with the fewest zeros,
  and then multiply the number of 1s by the number of 2s -- as a checksum on
  the data."
  []
  (let [tgt (->> (for [ly puzzle
                       :let [zs (sum (for [r ly] (count (filter #(= \0 %) r))))]]
                   [zs ly])
                 (sort-by first)
                 (first)
                 (last))]
    (* (sum (for [r tgt] (count (filter #(= \1 %) r))))
       (sum (for [r tgt] (count (filter #(= \2 %) r)))))))

(defn two
  "Function to decide the image data according to the layering rules. We are
  going to convert the 0s (black) to spaces, as that makes it a lot easier
  to read on the output."
  []
  (let [w (count (first (first puzzle)))
        h (count (first puzzle))]
    (for [row (range h)]
      (->> (for [col (range w)
                 :let [ld (map #(nth (nth % row) col) puzzle)
                       px (first (remove #(= \2 %) ld))]]
             (if (= \0 px) " " px))
           (apply str)))))
