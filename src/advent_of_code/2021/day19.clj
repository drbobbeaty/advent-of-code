(ns advent-of-code.2021.day19
  "Nineteenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum compact]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.set :refer [intersection]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take the scanner input and break it into a sequence of sequences
  of the scanner data so that it's much easier to process."
  [coll]
  (loop [src coll
         ans []
         row []]
    (if-let [l (first src)]
      (cond
        (cs/includes? l "---")
          (recur (rest src) ans row)
        (cs/includes? l ",")
          (let [pt (map parse-int (split l ","))]
            (recur (rest src) ans (conj row pt)))
        :else
          (recur (rest src) (conj ans row) []))
      (conj ans row))))

(def puzzle
  "This is the input of the scanner outputs from the probes."
  (-> (slurp "resources/2021/input/day19.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2021/input/day19-test.txt")
    (trim)
    (split #"\n")
    (parse)))

(def rotations
  "The set of all functions needed to translate a point from one orientation
  to any other possible orientation, and will be used to map sets of points
  to see if we have a match."
  (for [axes [identity            ;; look down each axis
              (fn [[x y z]] [(* -1 y) x z])
              (fn [[x y z]] [(* -1 x) (* -1 y) z])
              (fn [[x y z]] [y (* -1 x) z])
              (fn [[x y z]] [(* -1 z) y x])
              (fn [[x y z]] [z y (* -1 x)])]
        rotax [identity           ;; rotate through different "ups"
               (fn [[x y z]] [x z (* -1 y)])
               (fn [[x y z]] [x (* -1 y) (* -1 z)])
               (fn [[x y z]] [x (* -1 z) y])]]
    (comp axes rotax)))

(defn fit
  "Function to look at two scans, A, and B, and attempt to find the
  rotation and translation that takes B to A with at least 12 matching
  points. This is the key to putting this all together, and the determination
  of this was the first big challenge of the day."
  [sa sb]
  (let [mov (fn [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])
        ua (set sa)]
    (->> (for [rfn rotations
               :let [rotb (map rfn sb)]
               ia (range (count sa))
               ib (range (count rotb))
               :let [[xa ya za] (nth sa ia)
                     [xb yb zb] (nth rotb ib)
                     delta [(- xa xb) (- ya yb) (- za zb)]
                     movb (map #(mov % delta) rotb)
                     hits (intersection ua (set movb))]
               :when (<= 12 (count hits))]
           {:hits hits :rotfn rfn :delta delta})
      (distinct)
      (first))))

(defn distances
  "Function to compute the Manhattan Distances between every two points in
  the collection, and then return them as a set."
  [coll]
  (->> (for [[[ax ay az] [bx by bz]] (combinations coll 2)]
         (+ (abs (- ax bx)) (abs (- ay by)) (abs (- az bz))))
    (set)))

(defn matches
  "Function to calculate the scan matches by looking at the number of
  insersecting distances for the points within a set. If the number of
  matching distances exceeds the number needed (12), then we are in
  good shape, and can continue."
  [coll]
  (let [lim (count (combinations (range 12) 2))
        dl (map distances coll)
        dm (into {} (map vector (range) dl))]
    (for [i (range (count coll))
          j (range (inc i) (count coll))
          :when (and (not= i j) (<= lim (count (intersection (get dm i) (get dm j)))))]
      [i j])))

(defn orders
  "Function to order the scan process, and the translation mapping so that
  we compute the answers in the most efficient way possible. This was the
  key to geting this to solve in a reasonable time. We have to start with
  the '0' level, and then, do only those things that we already have a
  mapping to the '0' level for. Then simply repeat until the list is done."
  [pairs]
  (let [ord (atom [0])
        out (atom [])]
    (loop [todo pairs]
      (if-let [p (first todo)]
        (let [[a b] p]
          (cond
            (<= 0 (.indexOf @ord a))
              (do
                (swap! ord conj b)
                (swap! out conj [a b])
                (recur (rest todo)))
            (<= 0 (.indexOf @ord b))
              (do
                (swap! ord conj a)
                (swap! out conj [b a])
                (recur (rest todo)))
            :else
              (recur (concat (rest todo) [p]))))
        {:order @ord :folds @out}))))

(defn one
  "Function to find the total number of unique sensors in the trench.
  This is not just about finding the matches, but doing it in an order
  that makes it tractable, and not completely memory exhaustive."
  [& [coll]]
  (let [src puzzle
        mov (fn [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])
        hits (atom [])
        beac (atom [])
        {order :order folds :folds} (orders (matches src))]
    (infof "looking for matches and translations for %d scans" (count src))
    (doseq [[i j] folds
            :let [di (nth src i)
                  dj (nth src j)]]
      (swap! hits conj (assoc (fit di dj) :to i :from j)))
    (infof "calculated %d mappings" (count @hits))
    (doseq [lvl order
            :let [sd (nth src lvl)]]
      (loop [bs sd
             cl lvl]
        (if (zero? cl)
          (reset! beac (distinct (concat @beac bs)))
          (let [spt (filter #(= (:from %) cl) @hits)
                bdl (apply min (map :to spt))
                best (first (filter #(= (:to %) bdl) spt))
                rot (map (:rotfn best) bs)]
            (recur (map #(mov % (:delta best)) rot) bdl)))))
    (count @beac)))

(defn two
  "Function to find the largest manhattan distance between the sensors.
  This is very much like part 1, were we start with a origin, and then
  translate it to zero, and then it's all on the same reference axes."
  [& [coll]]
  (let [src puzzle
        mov (fn [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])
        hits (atom [])
        scan (atom [])
        {order :order folds :folds} (orders (matches src))]
    (infof "looking for matches and translations for %d scans" (count src))
    (doseq [[i j] folds
            :let [di (nth src i)
                  dj (nth src j)]]
      (swap! hits conj (assoc (fit di dj) :to i :from j)))
    (infof "calculated %d mappings" (count @hits))
    (doseq [lvl order]
      (loop [p [0 0 0]
             cl lvl]
        (if (zero? cl)
          (swap! scan conj {:lvl lvl :p p})
          (let [spt (filter #(= (:from %) cl) @hits)
                bdl (apply min (map :to spt))
                best (first (filter #(= (:to %) bdl) spt))
                rot ((:rotfn best) p)]
            (recur (mov rot (:delta best)) bdl)))))
    (last (sort (distances (map :p @scan))))))
