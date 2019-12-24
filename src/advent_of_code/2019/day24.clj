(ns advent-of-code.2019.day24
  "Twenty-fourth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum compact]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the scan of the bugs on the planet"
  ["....#"
   "#..#."
   "##..#"
   "#.###"
   ".####"])

(def trial1
  "Test data for part 1 - biodiversity of 2129920 on first repeat"
  ["....#"
   "#..#."
   "#..##"
   "..#.."
   "#...."])

(defn generation
  "Function to run the provided grid through a generation of the bugs so that
  we can see what the next minute will look like. This is a simple Game of Life
  type of scheme - where the rules are really in the conditional in the inner
  loop."
  [brd]
  (let [rows (count brd)
        cols (count (first brd))
        spc (fn [r c] (let [rd (nth brd r nil)] (nth rd c \.)))]
    (for [r (range rows)]
      (->> (for [c (range cols)
                 :let [me (spc r c)
                       bugs (->> [(spc (dec r) c) (spc (inc r) c) (spc r (inc c)) (spc r (dec c))]
                                 (filter #(= \# %))
                                 (count))]]
             (cond
               (and (= me \#) (not= bugs 1)) \.
               (and (= me \.) (<= 1 bugs 2)) \#
               :else                         me))
           (apply str)))))

(defn bio
  "Function to calculate the biodiversity of the grid by applying a power-of-2
  increase to each grid position - that's occupied by a bug. So we just need
  to note where the bugs are, 2^n that, and sum them all up."
  [brd]
  (->> (map vector (range) (apply str brd))
       (filter #(= \# (second %)))
       (map #(expt 2 (first %)))
       (sum)))

(defn one
  "Function to take the starting scan, and see what the biodiversity is at
  the point when the scan repeats itself."
  []
  (->> (loop [brd puzzle
              hist (transient #{puzzle})]
         (let [nxt (generation brd)]
           (info nxt)
           (if-not (hist nxt)
             (recur nxt (conj! hist nxt))
             nxt)))
       (bio)))

(defn folding
  "Function to handle the next generation of the foldable space where each
  grid is within another, and contains another, and so we have to put a blank
  one on the front and back, and then process all the elements accordingly.
  Then, we can remove any blank layers, and return to the caller the next
  generation of this folded universe."
  [bs]
  (let [epty ["....." "....." "..?.." "....." "....."]
        brds (concat [epty] bs [epty])
        spc (fn [b r c]
              (let [brd (nth brds b nil)]
                (let [rd (nth brd r nil)]
                  (nth rd c \.))))
        nbors (fn [b r c]
                (-> [(if (= 0 r) [(spc (dec b) 1 2) (spc b 1 c)])
                     (if (and (= 1 r) (not= 2 c)) [(spc b 0 c) (spc b 2 c)])
                     (if (and (= 1 r) (= 2 c)) [(spc b 0 c) (spc (inc b) 0 0) (spc (inc b) 0 1) (spc (inc b) 0 2) (spc (inc b) 0 3) (spc (inc b) 0 4)])
                     (if (and (= 2 r) (not= 2 c)) [(spc b 1 c) (spc b 3 c)])
                     (if (and (= 3 r) (= 2 c)) [(spc (inc b) 4 0) (spc (inc b) 4 1) (spc (inc b) 4 2) (spc (inc b) 4 3) (spc (inc b) 4 4) (spc b 4 c)])
                     (if (and (= 3 r) (not= 2 c)) [(spc b 2 c) (spc b 4 c)])
                     (if (= 4 r) [(spc b 3 c) (spc (dec b) 3 2)])
                     (if (= 0 c) [(spc (dec b) 2 1) (spc b r 1)])
                     (if (and (= 1 c) (not= 2 r)) [(spc b r 0) (spc b r 2)])
                     (if (and (= 1 c) (= 2 r)) [(spc b r 0) (spc (inc b) 0 0) (spc (inc b) 1 0) (spc (inc b) 2 0) (spc (inc b) 3 0) (spc (inc b) 4 0)])
                     (if (and (= 2 c) (not= 2 r)) [(spc b r 1) (spc b r 3)])
                     (if (and (= 3 c) (= 2 r)) [(spc (inc b) 0 4) (spc (inc b) 1 4) (spc (inc b) 2 4) (spc (inc b) 3 4) (spc (inc b) 4 4) (spc b r 4)])
                     (if (and (= 3 c) (not= 2 r)) [(spc b r 2) (spc b r 4)])
                     (if (= 4 c) [(spc b r 3) (spc (dec b) 2 3)])]
                    (compact)
                    (flatten)))
        vacuum (fn [s] (let [a (if (= epty (first s)) (rest s) s)]
                         (if (= epty (last a)) (drop-last a) a)))]
    (->> (for [b (range (count brds))]
           (for [r (range 5)]
             (->> (for [c (range 5)
                        :let [me (spc b r c)
                              bugs (->> (nbors b r c)
                                        (filter #(= \# %))
                                        (count))]]
                    (cond
                      (= 2 r c)                     \?
                      (and (= me \#) (not= bugs 1)) \.
                      (and (= me \.) (<= 1 bugs 2)) \#
                      :else                         me))
                  (apply str))))
         (vacuum))))

(defn two
  "Function to count all the bugs in 200 min of generations of the folded
  grid. After we grab the 200th generation, we then need to count up the
  number of bugs by just flattening the data and counting the bugs in the
  display."
  []
  (->> (nth (iterate folding [puzzle]) 200)
       (map #(apply str %))
       (apply str)
       (filter #(= \# %))
       (count)))
