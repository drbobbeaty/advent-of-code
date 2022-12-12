(ns advent-of-code.2022.day12
  "Twelfth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum ascii-a]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to parse the map into something we can work with as opposed to
  just the characters."
  [coll]
  (let [start (atom [])
        end (atom [])
        brd (atom [])]
    (doseq [[r row] (map vector (range) coll)]
      (->> (for [[c elev] (map vector (range) row)]
             (cond
               (= \S elev) (do (reset! start [r c]) 0)
               (= \E elev) (do (reset! end [r c]) 25)
               :else       (- (int elev) ascii-a)))
        (vec)
        (swap! brd conj)))
    {:start @start :end @end :board @brd}))

(def puzzle
  "This is the input of instructions for the CPU to drive the CRT in my
  communications device."
  (-> (slurp "resources/2022/input/day12.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["Sabqponm"
       "abcryxxl"
       "accszExk"
       "acctuvwj"
       "abdefghi"]
    (parse)))

(defn elev
  "Function to return the elevation of the ground based on the supplied board
  for the row 'r' and col 'c', provided."
  [brd [r c]]
  (nth (nth brd r) c))

(defn one
  "Function to find the minimum number of steps to get from the start to the
  end, given that we can't move up too steep a slope."
  [& [coll]]
  (let [{tgt :start loc :end brd :board} puzzle
        maxr (dec (count brd))
        maxc (dec (count (first brd)))
        visited (atom #{})]
    (loop [locs [{:loc loc :steps 0}]]
      (let [{[r c] :loc dist :steps} (first locs)]
        (if (= [r c] tgt)
          (first locs)
          (recur (concat
                   (rest locs)
                   (for [[dr dc] [[0 1] [-1 0] [0 -1] [1 0]]
                         :let [[nr nc] [(+ r dr) (+ c dc)]]
                         :when (and (<= 0 nr maxr) (<= 0 nc maxc)
                                    (<= (elev brd [r c]) (inc (elev brd [nr nc])))
                                    (not (@visited [nr nc])))]
                     (do
                       (swap! visited conj [nr nc])
                       {:loc [nr nc] :steps (inc dist)})))))))))

(defn two
  "Function to find the shorted path to the lowest point, not just the start."
  [& [coll]]
  (let [{tgt :start loc :end brd :board} puzzle
        maxr (dec (count brd))
        maxc (dec (count (first brd)))
        visited (atom #{})]
    (loop [locs [{:loc loc :steps 0}]]
      (let [{[r c] :loc dist :steps} (first locs)]
        (if (= 0 (elev brd [r c]))
          (first locs)
          (recur (concat
                   (rest locs)
                   (for [[dr dc] [[0 1] [-1 0] [0 -1] [1 0]]
                         :let [[nr nc] [(+ r dr) (+ c dc)]]
                         :when (and (<= 0 nr maxr) (<= 0 nc maxc)
                                    (<= (elev brd [r c]) (inc (elev brd [nr nc])))
                                    (not (@visited [nr nc])))]
                     (do
                       (swap! visited conj [nr nc])
                       {:loc [nr nc] :steps (inc dist)})))))))))
