(ns advent-of-code.2017.day13
  "Thirteenth day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(defn code-it
  "Function to parse the input from the puzzle into something that's reasonable
  to work with."
  [s]
  (cond
    (string? s) (let [[lvl dpth] (drop 1 (re-matches #"^(\d+): (\d+)$" s))
                      d (parse-int dpth)
                      sd (range d)]
                  {:level (parse-int lvl)
                   :depth d
                   :pos 0
                   :pos-seq (drop 1 (cycle (drop-last (concat sd (drop 1 (reverse sd))))))})
    (coll? s)   (map code-it s)
    :else       s))

(def puzzle
  "These are the channels of communication from one program to another. Each
  is a simple 1:many map, and we need to use this to discover the networks
  in the population."
  (-> (slurp "resources/2017/input/day13.txt")
    (cs/trim)
    (cs/split #"\n")
    (code-it)))

(def sample
  "This is the sample data from the puzzle."
  (-> ["0: 3"
       "1: 2"
       "4: 4"
       "6: 4"]
    (code-it)))

(defn tic
  "Function to take the firewall - and all it's scanners, and advance them one
  picosecond, and return the new state. This is very useful in the time
  evolution of the system."
  [scns]
  (for [s scns
        :let [ps (:pos-seq s)]]
    (assoc s :pos (first ps) :pos-seq (rest ps))))

(defn one
  "Function to find out the sum total of the severity of the times the packet
  is caught by a scanner - if it starts at t = 0."
  []
  (let [src puzzle
        maxl (apply max (map :level src))
        ding (loop [fw src
                    pos -1
                    hits []]
               (let [np (inc pos)
                     cell (first (filter #(and (= np (:level %)) (zero? (:pos %))) fw))]
                 (if (<= np maxl)
                   (recur (tic fw) (inc pos) (if cell (conj hits (:level cell)) hits))
                   hits)))]
    (->> (for [l ding
               :let [fwl (first (filter #(= l (:level %)) src))
                     l (:level fwl)
                     d (:depth fwl)]]
           {:level l :depth d :severity (* l d)})
      (map :severity)
      (apply +))))

(defn two
  "Function to calculate the *minimum* delay we need to have before starting
  a run through the firewall - in order to make it all the way through without
  getting nailed by one of the scanners. We use the sequences of each of the
  scanners, and then offset them by the travel time, and then when one has no
  zeros in it, we have a clean trip."
  []
  (let [src (->> puzzle
              (map #(dissoc % :pos-seq))
              (map #(assoc % :cycle (- (* 2 (:depth %)) 2))))
        mini (apply max (map :level src))]
    (loop [i mini]
      (let [scns (for [s src
                       :let [cyc (:cycle s)
                             idx (mod (+ i (:level s)) cyc)]]
                   (if (< idx (:depth s)) idx (- cyc idx)))]
        (if (some zero? scns)
          (recur (inc i))
          {:time i :state scns})))))
