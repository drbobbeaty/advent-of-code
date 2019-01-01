(ns advent-of-code.2017.day19
  "Thirteenth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def trial
  "This is the test input from the packet on the routing track."
  (-> (slurp "resources/2017/input/day19_trial.txt")
    (cs/split #"\n")
    (as-> s (map vec s))))

(def puzzle
  "This is the actusal input from the routing path for the packet."
  (-> (slurp "resources/2017/input/day19.txt")
    (cs/split #"\n")
    (as-> s (map vec s))))

(defn tget
  "Function to get the value of the track layout at column `x`, and row
  `y`. If nothing is there, a character space is returned - just to make
  the comparisons easier."
  [trk x y]
  (-> (nth trk y [])
    (nth x \space)))

(defn entry
  "Function to find the entry point for the track - this is always in the
  first row of the map, and it's a '|'... so let's find it to make the
  starting of the movements easier."
  [trk]
  (let [row (first trk)]
    [(.indexOf row \|) 0]))

(defn move-pkt
  "Function that takes a packet, and moves it along the track, as
  directed by the rules. It updates the direction, and the position,
  as well as the 'bits' it picks up along the way."
  [trk {dir :dir [x y] :pos b :bits s :steps :as pkt}]
  (let [[x' y'] (case dir
                  \v [x (inc y)]
                  \^ [x (dec y)]
                  \> [(inc x) y]
                  \< [(dec x) y])
        nts (tget trk x' y')
        [d' b'] (case dir
                  \v
                    (case nts
                      (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P
                       \Q \R \S \T \U \V \W \X \Y \Z) [dir (conj b nts)]
                      (\| \-) [dir b]
                      \+ (let [rt (tget trk (dec x') y')
                               lt (tget trk (inc x') y')]
                           (cond
                             (not= \space rt) [\< b]
                             (not= \space lt) [\> b]
                             :else [nil b]))
                      \space [nil b])
                  \^
                    (case nts
                      (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P
                       \Q \R \S \T \U \V \W \X \Y \Z) [dir (conj b nts)]
                      (\| \-) [dir b]
                      \+ (let [rt (tget trk (inc x') y')
                               lt (tget trk (dec x') y')]
                           (cond
                             (not= \space rt) [\> b]
                             (not= \space lt) [\< b]
                             :else [nil b]))
                      \space [nil b])
                  \>
                    (case nts
                      (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P
                       \Q \R \S \T \U \V \W \X \Y \Z) [dir (conj b nts)]
                      (\| \-) [dir b]
                      \+ (let [rt (tget trk x' (inc y'))
                               lt (tget trk x' (dec y'))]
                           (cond
                             (not= \space rt) [\v b]
                             (not= \space lt) [\^ b]
                             :else [nil b]))
                      \space [nil b])
                  \<
                    (case nts
                      (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P
                       \Q \R \S \T \U \V \W \X \Y \Z) [dir (conj b nts)]
                      (\| \-) [dir b]
                      \+ (let [rt (tget trk x' (dec y'))
                               lt (tget trk x' (inc y'))]
                           (cond
                             (not= \space rt) [\^ b]
                             (not= \space lt) [\v b]
                             :else [nil b]))
                      \space [nil b]))]
    {:dir d' :pos [x' y'] :bits b' :steps (if d' (inc s) s)}))

(defn one
  "Function to have the packet travel along the track, picking up the
  letters as it goes, and reporting those back to the game."
  []
  (loop [pkt {:dir \v :pos (entry puzzle) :bits [] :steps 1}]
    (let [nxt (move-pkt puzzle pkt)]
      (if (:dir nxt)
        (recur nxt)
        (update nxt :bits #(apply str %))))))
