(ns advent-of-code.2016.day15
  "Fifteenth day's solutions for the Advent of Code 2016"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def puzzle
  "This is the source list of all the discs and their initial positions."
  (-> "resources/2016/input/day15.txt"
      (io/reader)
      (line-seq)))

(defn code-it
  "Function to do the simple parsing of the input from the disc data. We really
  need to have it in a little better format in order to work with it."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))]
    (cond
      (string? s) (let [pts (->> (re-matches #"^Disc #(\d+) has (\d+) positions; at time=(\d+), it is at position (\d+)\.$" s)
                              (drop 1)
                              (map pint))]
                    {:disc (first pts)
                     :size (nth pts 1)
                     :time (nth pts 2)
                     :pos (last pts)})
      (coll? s)   (map code-it s)
      :else       s)))

(defn mk-times
  "Function to create an infinite sequence of the disc positions for each time
  segment - starting at the offset to align all the discs in time as they
  rotate."
  [d cnt]
  (flatten
    (conj
      (vec (repeat (- cnt (:disc d)) false))
      (drop (:pos d) (flatten (repeat (range (:size d))))))))

(defn run
  "Function to take a sequence of disc descriptions and create their time
  series of positions and offset them to line up so that the time thru the
  machine appears to take no time. So we can then look for the discs to just
  line up - which we do, and then filter them to find the first one that
  allows safe passage."
  [ds]
  (let [cnt (count ds)]
    (->> (apply map vector (map #(- % cnt) (range)) (map #(mk-times % cnt) ds))
      (filter (comp not neg? first))
      (filter #(apply = 0 (rest %)))
      (first))))

(defn one
  "Function to find when I  can push the button to get a prize from the machine."
  [& [src]]
  (run (code-it (or src puzzle))))

(defn two
  "Function to find when I  can push the button to get a prize from the machine."
  [& [src]]
  (let [old (code-it (or src puzzle))
        ds (conj (vec old) {:disc (inc (count old)) :size 11 :time 0 :pos 0})]
    (run ds)))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src (code-it ["Disc #1 has 5 positions; at time=0, it is at position 4."
                      "Disc #2 has 2 positions; at time=0, it is at position 1."])
        cnt (count src)]
    (run src)))
