(ns advent-of-code.2017.day14
  "Fourteenth day's solutions for the Advent of Code 2017"
  (require [advent-of-code.2017.day10 :as d10]
           [advent-of-code.util :refer [parse-int]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(def puzzle
  "This is the code that we will use, with knot, to build the disk pattern."
  "jxqlasbh")

(def sample
  "This is the sample data from the puzzle."
  "flqrgnkx")

(def blocks
  "These are the mappings from the hex characters to the block patterns
  on the disk. This just makes the conversion from the output of `knot`
  to the disk block pattern."
  {\0 "...."
   \1 "...#"
   \2 "..#."
   \3 "..##"
   \4 ".#.."
   \5 ".#.#"
   \6 ".##."
   \7 ".###"
   \8 "#..."
   \9 "#..#"
   \a "#.#."
   \b "#.##"
   \c "##.."
   \d "##.#"
   \e "###."
   \f "####"})

(defn knot
  "Function to conveniently call the `knot` function from Day 10, with
  all the preparation needed to return what we needed."
  [s]
  (d10/knot (range 256) (concat (map int s) [17 31 73 47 23])))

(defn disk
  "Function to create the disk pattern as a vector of vectors for easy
  manipulation of the data."
  [arg]
  (->> (map vector (repeat arg) (range 128))
    (map #(str (first %) "-" (second %)))
    (map knot)
    (mapv (fn [s] (vec (apply str (map #(get blocks %) s)))))))

(defn dget
  "Function to get the value of the disk block in the display at row `r`,
  and column `c`."
  [da r c]
  (-> (nth @da r [])
      (nth c nil)))

(defn dset
  "Function to set the value of the disk block at row `r`, and column `c`,
  to the value `v`."
  [da r c v]
  (reset! da (-> (nth @da r [])
               (assoc c v)
               (->> (assoc @da r)))))

(defn fill
  "Function to fill in the disk full blocks in proximity to the given row and
  column. Return a sequence of all the points, so that the caller can do what
  they want with them. As it checks each block, it will replace it with an
  'X' so that it's not counted again."
  [da r c]
  (let [hits (atom [])]
    (when (= \# (dget da r c))
      (swap! hits conj [r c])
      (dset da r c \X)
      (swap! hits concat (fill da r (inc c)))
      (swap! hits concat (fill da r (dec c)))
      (swap! hits concat (fill da (inc r) c))
      (swap! hits concat (fill da (dec r) c))
      @hits)))

(defn one
  "Function to get the structure of the disk, and then count up the used
  blocks on the disk, and return that."
  []
  (apply + (map (fn [s] (count (filter #(= \# %) s))) (disk puzzle))))

(defn two
  "Function to find the number of contiguous groups in the disk based on
  the blocks being full. Find them all, and return the count."
  []
  (let [dsk (atom (disk puzzle))
        grps (atom 0)]
    (doseq [c (range 128)
            r (range 128)
            :when (= \# (dget dsk r c))]
      (fill dsk r c)
      (swap! grps inc))
    @grps))
