(ns advent-of-code.2021.day22
  "Twenty-second day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a line of the reboot sequence and turn it into a nice
  parsed map with all the values more easily used by the rest of the work."
  [s]
  (let [[_ tog xl xh yl yh zl zh] (re-matches #"^(.+?) x=([\-0-9]+)..([\-0-9]+),y=([\-0-9]+)..([\-0-9]+),z=([\-0-9]+)..([\-0-9]+)$" s)
        xl (parse-int xl)
        xh (parse-int xh)
        yl (parse-int yl)
        yh (parse-int yh)
        zl (parse-int zl)
        zh (parse-int zh)]
    {:toggle (keyword tog)
     :x [xl xh]
     :y [yl yh]
     :z [zl zh]
     :init (and (<= -50 xl xh 50) (<= -50 yl yh 50) (<= -50 zl zh 50))}))

(def puzzle
  "This is the input of the reboot instructions for the reactor core, and
  that's what we have to use to get online."
  (-> (slurp "resources/2021/input/day22.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2021/input/day22-test.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(defn toggle
  "Function to take a map of the reactor core, and it's states, and update
  that state with the reboot instruction passed in."
  [core {tog :toggle [xl xh] :x [yl yh] :y [zl zh] :z}]
  (into core
    (for [x (range xl (inc xh)) y (range yl (inc yh)) z (range zl (inc zh))]
      [[x y z] tog])))

(defn one
  "Function to find the number of cubes 'on' in the initial reboot steps
  if we only look at the 'initialization steps' - and not the big ones."
  [& [coll]]
  (loop [stps (filter :init puzzle)
         core {}]
    (if-let [s (first stps)]
      (recur (rest stps) (toggle core s))
      {:on (count (for [[k v] core :when (= :on v)] k))})))

(defn two
  "Function to find the number of cubes on - for the entire reboot sequence."
  [& [coll]]
  (loop [stps puzzle
         hits []]
    (if-let [{atog :toggle [axl axh] :x [ayl ayh] :y [azl azh] :z :as sa} (first stps)]
      (let [nhits (for [{btog :toggle [bxl bxh] :x [byl byh] :y [bzl bzh] :z :as sb} hits
                       :let [xlo (max axl bxl) xhi (min axh bxh)]
                       :when (<= xlo xhi)
                       :let [ylo (max ayl byl) yhi (min ayh byh)]
                       :when (<= ylo yhi)
                       :let [zlo (max azl bzl) zhi (min azh bzh)]
                       :when (<= zlo zhi)]
                   {:x [xlo xhi] :y [ylo yhi] :z [zlo zhi] :toggle (if (= :on btog) :off :on)})]
        (recur (rest stps) (concat hits (if (= :on atog) (conj nhits sa) nhits))))
      (reduce
        (fn [acc {tog :toggle [xl xh] :x [yl yh] :y [zl zh] :z :as s}]
          (+ acc (* (inc (- xh xl)) (inc (- yh yl)) (inc (- zh zl)) (if (= :on tog) 1 -1)))
        ) 0 hits))))
