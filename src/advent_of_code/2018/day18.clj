(ns advent-of-code.2018.day18
  "Eighteenth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the forest."
  (-> (slurp "resources/2018/input/day18.txt")
      (cs/split #"\n")
      (as-> s (map vec s))))

(def sample
  "This is the sample forest in the puzzle statement."
  (map vec [".#.#...|#."
            ".....#|##|"
            ".|..|...#."
            "..|#.....#"
            "#.#|||#|#|"
            "...#.||..."
            ".|....|..."
            "||...#|.#|"
            "|.||||..|."
            "...#.|..|."]))

(defn neighbors
  "Function to return the sequence of neighbors in the forest at the provided
  point. The edges of the forest are just spaces, and don't for anything in
  this case."
  [fd x y]
  (let [fsq (fn [i j] (-> (nth fd y []) (nth x \space)))
        grp (atom [])
        h (count fd)
        w (count (first fd))]
    (for [j (range (max 0 (dec y)) (inc (min (dec h) (inc y))))
          i (range (max 0 (dec x)) (inc (min (dec w) (inc x))))
          :when (not= [x y] [i j])]
      (-> (nth fd j []) (nth i \space)))))

(defn next-gen
  "Function to compute the next generation of a point in the forest, as well
  as the next generation of the entire forest. This is just a nice use of
  arity to keep it all in one place."
  ([fd]
    (let [h (count fd)
          w (count (first fd))]
      (vec
        (for [[y row] (map vector (range) fd)]
          (vec
            (for [[x old] (map vector (range) row)]
              (next-gen fd x y)))))))
  ([fd x y]
    (let [curr (-> (nth fd y []) (nth x \space))
          nbrs (neighbors fd x y)
          tc (count (filter #(= \| %) nbrs))
          lyc (count (filter #(= \# %) nbrs))]
      (case curr
        \. (if (<= 3 tc) \| \.)
        \| (if (<= 3 lyc) \# \|)
        \# (if (and (pos? lyc) (pos? tc)) \# \.)))))

(defn- inven
  "Function to count the number of specific acres in the forest."
  [fd c]
  (apply + (map (fn [r] (count (filter #(= c %) r))) fd)))

(defn woods
  "Function to count the number of wooded acres in the forest."
  [fd]
  (inven fd \|))

(defn yards
  "Function to count the number of wooded acres in the forest."
  [fd]
  (inven fd \#))

(defn show
  "Function to simply make a decent looking output for the 'forest' based on
  the state of that forest passed in."
  [fd]
  (doseq [r fd]
    (println (apply str r))))

(defn flog
  "Function to make a nice log message for the provided forest. This
  will make it easy to see what's happening."
  [fd]
  (let [fmt (fn [s] (format "%s" (apply str s)))]
    (info (str "forest:\n" (cs/join "\n" (map fmt fd))))))

(defn one
  "Function to compute the resource value after 10 minutes."
  []
  (let [ep (first (drop 10 (iterate next-gen puzzle)))]
    (* (woods ep) (yards ep))))

(defn two
  "Function to return the resource utilization for any number of generations
  that exceeds 3000. There is a pattern that develops, and this is the cycle
  of the values as that progression continues. We simply have to map the
  desired generation into one of these values."
  [n]
  (let [cyc [177815 182358 186042 192504 195308 200646 205120 208650
             210588 212833 212688 212443 208278 200466 196680 195290
             189980 186795 184392 180560 181383 182700 180942 176782
             175212 173290 173658 173922]
        idx (mod (- n 3001) 28)]
    [idx (nth cyc idx)]))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [efs (drop 3000 (iterate next-gen puzzle))
        ]
    (doseq [[i fs] (take 100 (map vector (range 3000 5000) efs))]
      (infof "iteration %s ... resource value: %s" i (* (woods fs) (yards fs))))))
