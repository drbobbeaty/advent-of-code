(ns advent-of-code.2018.day14
  "Fourteenth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn bake
  "Function to take the state of the recipies and the elves, and bake one more
  round of recipies with the elves, updating the output of the baking for the
  recipies and the elves. The state is passed in, and returned like:
    {:elf-1 5 :elf-2 3 :recipes [3 7 1 0]}
  where the elements are pretty much just what the puzzle describes."
  [{e1 :elf-1 e2 :elf-2 rs :recipes :as arg}]
  (let [oven (fn [a b] (let [s (+ a b)
                             td (quot s 10)
                             od (- s (* 10 td))]
                         (if (pos? td) [td od] [od])))
        e1r (nth rs e1)
        e2r (nth rs e2)
        nrs (apply conj rs (oven e1r e2r))
        nrsc (count nrs)]
    {:elf-1 (mod (+ e1 (inc e1r)) nrsc)
     :elf-2 (mod (+ e2 (inc e2r)) nrsc)
     :recipes nrs}))

(defn one
  "Function to calculate the next ten recipe values after a given number of
  them have been done by the elves."
  [& [n]]
  (let [tgt (or n 505961)
        game (first (drop tgt (iterate bake {:elf-1 0 :elf-2 1 :recipes [3 7]})))]
    (apply str (take 10 (drop tgt (:recipes game))))))

(defn two
  "Function to find out how many recipies have to be tried before the given
  sequence appears in the sequence of recipies. The key in this one is that
  we can 'jump' in the sequence by computing 500,000 at a time, and then
  doing the costly checks, and that makes it a lot faster."
  [& [rs]]
  (let [tc (or rs "505961")
        burn (fn [g] (first (drop 500000 (iterate bake g))))]
    (loop [gb (burn {:elf-1 0 :elf-2 1 :recipes [3 7]})]
      (infof "len=%s ...baking..." (count (:recipes gb)))
      (let [hit (.indexOf (apply str (:recipes gb)) tc)]
        (if (neg? hit)
          (recur (burn gb))
          [tc hit])))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [game (first (drop 3000 (iterate bake {:elf-1 0 :elf-2 1 :recipes [3 7]})))
        grs (apply str (:recipes game))]
    (for [tc ["51589" "01245" "92510" "59414"]]
      [tc (.indexOf grs tc)])))
