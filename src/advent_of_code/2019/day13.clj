(ns advent-of-code.2019.day13
  "Thirteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program for the game."
  (-> (slurp "resources/2019/input/day13.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn trip
  "Function to run the supplied program until it halts, or there are at
  least three values in the output array. This is because we want to be
  able to deal with these values as a set, and this just makes the calling
  a little easier."
  [cpu]
  (let [ini (if (map? cpu) cpu {:memory cpu})]
    (loop [mem (run (assoc ini :io-wait true))]
      (if-not (or (#{:halt :io-in} (:state mem)) (= 3 (count (:output mem))))
        (recur (run (assoc mem :io-wait true)))
        mem))))

(defn one
  "Function to run the program, and based on the output, count the number of
  block tiles that will appear on the screen."
  [& [coll]]
  (->> (loop [cpu (or coll puzzle)
              blks (transient [])]
         (let [nxt (trip cpu)
               [a b c] (:output nxt)]
           (if (not= (:state nxt) :halt)
             (recur (assoc nxt :output []) (conj! blks [[a b] c]))
             (persistent! (if (and a b c) (conj! blks [[a b] c]) blks)))))
       (filter #(= 2 (last %)))
       (count)))

(defn render
  "Function to render the display of the game, as defined in the puzzle for
  walls, blocks, paddle, and ball. This will also put the segment display
  score as the first element in the return sequence."
  [cpu tiles cnt]
  (let [scr (get tiles [-1 0])
        bts (keys tiles)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))]
    (concat [(merge {:score scr :turns cnt} (select-keys cpu [:state]))]
      (for [r (range rows)
            :let [rd (for [[k v] tiles :when (= r (second k))] [k v])]]
          (->> (sort-by (comp first first) rd)
               (map second)
               (map #(get " W*-O" %))
               (apply str))))))

(defn turn
  "Function to 'take a turn' on the video game, where the output will be
  returned, and the new cpu state. This is a convenient method to handle
  all the updates that will be done in the second part of the solution."
  [cpu]
  (loop [mem cpu
         blks (transient {})]
    (let [nxt (trip mem)
          [a b c] (:output nxt)]
      (if-not (#{:halt :io-in} (:state nxt))
        (recur (assoc nxt :output []) (assoc! blks [a b] c))
        {:cpu nxt :output (persistent! (if (and a b c) (assoc! blks [a b] c) blks))}))))

(defn two
  "Function to 'play the game' and the trick is that the control of the
  paddle is everything. I've tried manually doing it - and it gets into a
  cycle and never finishes... I've tried projecting the line of the ball
  to the plane of the paddle - misses... then I saw that a simple call to
  the compare function on the x-axis values of the ball and the paddle
  does exactly what I needed. I was stumped until that."
  [& [coll]]
  (loop [cpu (or coll (assoc (vec puzzle) 0 2))
         brd {}
         cnt 0]
    (let [eot (turn cpu)
          nb (merge brd (:output eot))
          pp (first (for [[k v] nb :when (= 3 v)] k))
          cbp (first (for [[k v] nb :when (= 4 v)] k))
          pi (compare (first cbp) (first pp))]
      (when (or (zero? (mod cnt 500)) (= (:state (:cpu eot)) :halt))
        (doseq [l (render (:cpu eot) nb cnt)] (info l))
        (infof "paddle: %s %s -> %d" cbp pp pi))
      (if (= (:state (:cpu eot)) :io-in)
        (recur (update (:cpu eot) :input concat [pi]) nb (inc cnt))
        (get nb [-1 0])))))
