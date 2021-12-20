(ns advent-of-code.2021.day20
  "Twentieth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Take the input as a sequence of lines, and parse it into an enhancement
  algo and the input image. At the start, we know the void around the image
  is jut '.' - so start there."
  [coll]
  {:algo (first coll) :image (drop 2 coll) :void \.})

(def puzzle
  "This is the input of the image data from all the scanners we sent out."
  (-> (slurp "resources/2021/input/day20.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2021/input/day20-test.txt")
    (trim)
    (split #"\n")
    (parse)))

(defn bindec
  "Function to take a binary sequence and turn it into a decimal number."
  [bs]
  (loop [bits (reverse bs)
         base 1
         sum 0]
    (if-let [b (first bits)]
      (recur (rest bits) (* 2 base) (+ sum (* base b)))
      sum)))

(defn pass
  "Take an image, an infinite void color, and an enhancement algo, and work
  to make a new image based on these components. At the same time, update
  the void as it will be effected by the algo - and could oscillate from on
  to off, and back."
  [{algo :algo img :image void :void}]
  (let [tripv (str void void void)
        pady (repeat 3 (apply str (repeat (+ 6 (count (first img))) void)))
        padx (map #(str tripv % tripv) img)
        pad (concat pady padx pady)
        hpad (count pad)
        wpad (count (first pad))
        nxt (for [r (range 1 (dec hpad))]
              (->> (for [c (range 1 (dec wpad))
                         :let [t (take 3 (drop (dec c) (nth pad (dec r))))
                               m (take 3 (drop (dec c) (nth pad r)))
                               b (take 3 (drop (dec c) (nth pad (inc r))))
                               code (bindec (map #(if (= \# %) 1 0) (concat t m b)))]]
                     (nth algo code))
                (apply str)))
        lit (count (filter #(= \# %) (apply concat nxt)))
        nvoid (nth algo (bindec (map #(if (= \# %) 1 0) (repeat 9 void))))]
    {:image nxt :algo algo :lit lit :size [wpad hpad] :void nvoid}))

(defn one
  "Function to find the number of lit pixels after two passes of the
  enhancement algo on the image."
  [& [coll]]
  (pass (pass puzzle)))

(defn two
  "Function to find the number of lit pixels after 50 passes of the
  enhancement algo on the image."
  [& [coll]]
  (loop [img puzzle
         cnt 50]
    (if (pos? cnt)
      (recur (pass img) (dec cnt))
      img)))
