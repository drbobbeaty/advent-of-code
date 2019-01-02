(ns advent-of-code.2017.day21
  "Twenty-first day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.util :refer [split]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the list of particle characteristics that are in the storm."
  (-> (slurp "resources/2017/input/day21.txt")
    (split "\n")
    (as-> s (remove cs/blank? s))))

(def sample
  "This is the sample mapping data in the puzzle."
  ["../.# => ##./#../..."
   ".#./..#/### => #..#/..../..../#..#"])

(defn ->str
  "Function to take an array of arrays of characters like: [[. .][. #]] and
  return the standard single string representation: '../.#'."
  [sa]
  (if (coll? sa)
    (cs/join "/" (map #(apply str %) sa))
    sa))

(defn ->arr
  "Simple funciton to convert a string of the form '../.#' to an array of
  arrays of characters: [[. .][. #]] for mapping purposes."
  [s]
  (if (string? s)
    (map vec (split s "/"))
    s))

(defn rot
  "Function to take a input pic map, 2x2 or 3x3, and rotate it three times
  and return a sequence of *unique* values that these, and the original
  create. If it's symmetric, this saves a lot of processing."
  [s]
  (let [inp (if (string? s) (->arr s) s)]
    (->> (case (count inp)
           2 (let [[[a b][c d]] inp]
               [[[a b][c d]]
                [[c a][d b]] [[d c][b a]] [[b d][a c]]])
           3 (let [[[a b c][d e f][g h i]] inp]
               [[[a b c][d e f][g h i]]
                [[g d a][h e b][i f c]] [[i h g][f e d][c b a]] [[c f i][b e h][a d g]]]))
      (map ->str)
      (distinct))))

(defn flp
  "Function to take a input pic map, 2x2 or 3x3, and flip it horizontally
  and vertically and return a sequence of *unique* values that these, and
  the original create. If it's symmetric, this saves a lot of processing."
  [s]
  (let [inp (if (string? s) (->arr s) s)]
    (->> (case (count inp)
           2 (let [[[a b][c d]] inp]
               [[[a b][c d]]
                [[c d][a b]] [[b a][d c]]
                [[d c][b a]]])
           3 (let [[[a b c][d e f][g h i]] inp]
               [[[a b c][d e f][g h i]]
                [[g h i][d e f][a b c]] [[c b a][f e d][i h g]]
                [[i h g][f e d][c b a]]]))
    (map ->str)
    (distinct))))

(defn equiv
  "Function to take a string representing a 2x2 or 3x3 picture cell and
  return all the permutations of it - flipped, rotated, etc. that are
  considered equivalent in the rules. This can then be used to find an
  expansion string."
  [s]
  (->> (rot s)
    (map flp)
    (flatten)
    (distinct)))

(defn code-it
  "Function to parse the input from the puzzle and parse it into a useful
  data structure that we can use later in the processing. This will also
  include all equivalent mappings so that there is complete coverage in
  the output for all cases."
  [s]
  (let [raw (into {} (map #(vec (drop 1 (re-matches #"^(.+) => (.+)$" %))) s))]
    (loop [ks (keys raw)
           m raw]
      (if-let [k (first ks)]
        (recur (rest ks) (merge (zipmap (equiv k) (repeat (get m k))) m))
        m))))

(defn build
  "Function to create a new 'display' with the provided width and height. This
  will return the vector-of-vectors with all the pixels 'off'. Just ready to
  have the new data placed on it."
  [w h]
  (let [row (vec (repeat w \.))]
    (vec (repeat h row))))

(defn dget
  "Function to get the value of the pixel in the (atom) display at row `r`,
  and column `c`."
  [d r c]
  (-> (nth @d r [])
    (nth c nil)))

(defn dset
  "Function to set the pixel at row `r`, and column `c`, to the value `v`
  in the display atom 'd'."
  [d r c v]
  (reset! d (-> (nth @d r [])
              (assoc c v)
              (->> (assoc @d r)))))

(defn bget
  "Function to look at the picture array - a vector-of-vectors representation
  of the image, and return the 'block' in that image starting at the offset
  [i j] w.r.t. the 'bsz'. This will return a 'bsz' picture array that we can
  then process as needed."
  [pa j i bsz]
  (let [sr (* j bsz)
        sc (* i bsz)]
    (for [r (range bsz)
          :let [row (nth pa (+ r sr) [])]]
      (take bsz (drop sc row)))))

(defn lit
  "Function to take a picture array or string, and count the 'lit' pixels
  in the image based on the puzzle rules. This is just a simple way to get
  the answer from either of the representations of the data."
  [pa]
  (let [ips (if (string? pa) pa (->str pa))]
    (count (filter #(= \# %) ips))))

(defn expand
  "Function to expand the incoming picture based on the expansion rules in
  the puzzle."
  [em pa]
  (let [ipa (if (string? pa) (->arr pa) pa)
        sz (count ipa)
        [bsz nbz] (if (even? sz) [2 3] [3 4])
        nsz (* (quot sz bsz) nbz)
        dis (atom (build nsz nsz))]
    (doseq [j (range (quot sz bsz))
            i (range (quot sz bsz))
            :let [blk (bget ipa j i bsz)
                  nblk (->arr (get em (->str blk)))
                  sr (* j nbz)
                  sc (* i nbz)]
            r (range nbz)
            c (range nbz)]
      (dset dis (+ r sr) (+ c sc) (nth (nth nblk r []) c \.)))
    (->str @dis)))

(defn one
  "Function to look at the number of pixels lit after 5 iterations."
  []
  (let [em (code-it puzzle)]
    (loop [pic ".#./..#/###"
           cnt 0]
      (if (< cnt 5)
        (recur (expand em pic) (inc cnt))
        {:pic pic :size (count (->arr pic)) :lit (lit pic)}))))

(defn two
  "Function to look at the number of pixels lit after 18 iterations. My
  current solution of 2418435 is 'too high'... looking at the code..."
  []
  (let [em (code-it puzzle)]
    (loop [pic ".#./..#/###"
           cnt 0]
      (if (< cnt 18)
        (recur (expand em pic) (inc cnt))
        {:size (count (->arr pic)) :lit (lit pic)}))))

(defn yoyo
  "This is just a function that we use in the building to see that the
  individual parts are working."
  []
  (let [em (code-it sample)
        ef (partial expand em)
        fit (first (drop 2 (iterate ef ".#./..#/###")))]
    {:pic fit :size (count (->arr fit)) :lit (lit fit)}))
