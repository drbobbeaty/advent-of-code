(ns advent-of-code.2018.day06
  "Sixth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]))

(def puzzle
  "This is the source list of all elf fabric claims."
  (->> "resources/2018/input/day06.txt"
    (io/reader)
    (line-seq)
    (map (fn [s] (map #(Integer/parseInt %) (cs/split s #", "))))))

(defn closest
  "Function to take a point, and a sequence of points and find the closest
  point in the sequence to the provided point - if it's unique. If there are
  more than one point in the sequence with the same distance, return `nil`."
  [p sp]
  (let [df (fn [[a b] [c d]] (+ (Math/abs (- a c)) (Math/abs (- b d))))
        ds (for [ta sp] {:tst p :anchor ta :dist (df p ta)})
        mind (apply min (map :dist ds))
        hits (filter #(= (:dist %) mind) ds)]
    (if (= 1 (count hits))
      (first hits))))

(defn all-dist
  "Function to take a point, and a sequence of points and find the sum total
  distance fron that point to *all* the points in the sequence. This sum will
  then be returned."
  [p sp]
  (let [df (fn [[a b] [c d]] (+ (Math/abs (- a c)) (Math/abs (- b d))))]
    (apply + (map #(df p %) sp))))

(defn one
  "Function to calculate the largest, non-infinite, area that's closest to a
  single point provided in the puzzle. The points to exclude are found by
  their proximity to the perimeter - and are identified as we run through
  the space attributing points to each anchor point."
  [& [src]]
  (let [sap (or src puzzle)
        [mx my] [(inc (apply max (map first sap))) (inc (apply max (map second sap)))]
        excl (atom #{})
        edge? (fn [[x y]] (or (= x 0) (= x mx) (= y 0) (= y my)))
        hits (atom {})
        bump (fn [m k] (if (get m k) (update m k inc) (assoc m k 1)))]
    (doseq [p (for [x (range (inc mx)) y (range (inc my))] [x y])
            :let [cpm (closest p sap)
                  ap (:anchor cpm)]
            :when ap]
      (if (edge? p) (swap! excl conj ap))
      (swap! hits bump ap))
    (doseq [ep @excl]
      (swap! hits dissoc ep))
    (apply max (vals @hits))))

(defn two
  "Function to find the region where the combined distance to all points in
  the puzzle is less than 10,000 - and count them."
  [& [src]]
  (let [sap (or src puzzle)
        [mx my] [(inc (apply max (map first sap))) (inc (apply max (map second sap)))]
        hits (atom [])
        bump (fn [m k] (if (get m k) (update m k inc) (assoc m k 1)))]
    (doseq [p (for [x (range (inc mx)) y (range (inc my))] [x y])
            :let [td (all-dist p sap)]
            :when (< td 10000)]
      (swap! hits conj p))
    (count @hits)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]]
        [mx my] [(inc (apply max (map first src))) (inc (apply max (map second src)))]
        excl (atom #{})
        edge? (fn [[x y]] (or (= x 0) (= x mx) (= y 0) (= y my)))
        hits (atom {})
        bump (fn [m k] (if (get m k) (update m k inc) (assoc m k 1)))]
    (doseq [p (for [x (range (inc mx)) y (range (inc my))] [x y])
            :let [cpm (closest p src)
                  ap (:anchor cpm)]
            :when ap]
      (if (edge? p) (swap! excl conj ap))
      (swap! hits bump ap))
    (doseq [ep @excl]
      (swap! hits dissoc ep))
    (apply max (vals @hits))))

(defn bobo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]]
        [mx my] [(inc (apply max (map first src))) (inc (apply max (map second src)))]
        hits (atom [])
        bump (fn [m k] (if (get m k) (update m k inc) (assoc m k 1)))]
    (doseq [p (for [x (range (inc mx)) y (range (inc my))] [x y])
            :let [td (all-dist p src)]
            :when (< td 32)]
      (swap! hits conj p))
    (count @hits)))
