(ns advent-of-code.2018.day03
  "Third day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.math.combinatorics :as cmc]))

(def puzzle
  "This is the source list of all elf fabric claims."
  (-> "resources/2018/input/day03.txt"
      (io/reader)
      (line-seq)))

(defn code-it
  "Function to do the simple parsing of the input from the elf descriptions to
  a more intelligent, coordinate system so that the comparisons are done much
  more simply."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))]
    (cond
      (string? s) (let [pts (->> (re-matches #"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" s)
                              (drop 1)
                              (map pint))]
                    {:id (first pts)
                     :patch (rest pts)})
      (coll? s)   (map code-it s)
      :else       s)))

(defn overlap
  "Function to return the overlap, as a tuple [x y w h], of the two rectangles.
  If there is no overlap, then this function returns `nil`."
  [[x1 y1 w1 h1] [x2 y2 w2 h2]]
  (let [w (max 0 (- (min (+ x1 w1) (+ x2 w2)) (max x1 x2)))
        h (max 0 (- (min (+ y1 h1) (+ y2 h2)) (max y1 y2)))]
    (if (and (pos? w) (pos? h))
      [(max x1 x2) (max y1 y2) w h])))

(defn hits
  "Function to take a sequence of intersection rectangles and compute the
  union area of all the rectangles. This is what the puzzle wants, so if the
  same area appears in multiple intersection rectangles, then it's only
  counted once."
  [rs fw fh]
  (let [fab (atom (vec (repeat fh (vec (repeat fw 0)))))
        pip (fn [x y] (reset! fab (-> (nth @fab y [])
                                    (assoc x 1)
                                    (->> (assoc @fab y)))))
        pir (fn [[x y w h]] (doseq [x' (map #(+ x %) (range w))
                                    y' (map #(+ y %) (range h))]
                              (pip x' y')))]
      (doseq [r rs] (pir (second r)))
      (apply + (map #(apply + %) @fab))))

(defn one
  "Function to count all the overlapping sections of material on the elf's
  different designs so that we get an idea of now much of a problem this is."
  [& [src]]
  (let [pids (code-it (or src puzzle))]
    (-> (for [[a b] (cmc/combinations pids 2)]
          [[(:id a) (:id b)] (overlap (:patch a) (:patch b))])
      (as-> s (filter second s))
      (hits 1500 1500))))

(defn two
  "Function to look at all the elf swatch IDs, and find the one (or many)
  that have *no* overlaps with any of the other swatches. This will just
  look at the overlaps, and then remove those from the list of all of
  them in the data set."
  [& [src]]
  (let [pids (code-it (or src puzzle))
        all (atom (set (map :id pids)))]
    (doseq [[a b] (cmc/combinations pids 2)
            :let [ov (overlap (:patch a) (:patch b))]
            :when ov]
      (swap! all disj (:id a) (:id b)))
    @all))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src (code-it ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])]
    (-> (for [[a b] (cmc/combinations src 2)]
          [[(:id a) (:id b)] (overlap (:patch a) (:patch b))])
      (as-> s (filter second s))
      (hits 8 8))))
