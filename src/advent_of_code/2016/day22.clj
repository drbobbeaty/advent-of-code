(ns advent-of-code.2016.day22
  "Twenty-second day's solutions for the Advent of Code 2016"
  (require [clojure.java.io :as io]
           [clojure.math.combinatorics :as cmc]
           [clojure.string :as cs]))

(def puzzle
  "This is the source of the disk usage on all the nodes in the grid."
  (->> "resources/2016/input/day22.txt"
       (io/reader)
       (line-seq)))

(defn code-it
  "Function to do the simple parsing of the input of the rules for scrambling
  the password."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))]
    (cond
      (string? s) (if (.startsWith s "/dev/grid/node")
                    (let [pts (->> (re-matches #"^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%$" s)
                                (drop 1)
                                (map pint))]
                      {:loc [(first pts) (second pts)]
                       :size (nth pts 2)
                       :used  (nth pts 3)
                       :avail (nth pts 4)
                       :pct (nth pts 5)}))
      (coll? s)   (remove nil? (map code-it s))
      :else       s)))

(defn one
  "Function to count the 'viable pairs' in the disk array by looking at each
  possible pair, and seeing if they meet the criteria for the puzzle."
  [& [src]]
  (let [nds (code-it (or src puzzle))]
    (-> (for [[a b] (cmc/combinations nds 2)
              :when (or
                      (and (pos? (:used a)) (<= (:used a) (:avail b)))
                      (and (pos? (:used b)) (<= (:used b) (:avail a))))]
          [(:loc a) (:loc b)])
        (count))))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  nil)
