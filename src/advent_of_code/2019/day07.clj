(ns advent-of-code.2019.day07
  "Seventh day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.math.combinatorics :as cmc]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the Intcode program for the amplifiers."
  (-> (slurp "resources/2019/input/day07.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(def trial1
  "Test program with phase settings yields 43210 in part 1"
  {:mem [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0] :phase [4 3 2 1 0]})

(def trial2
  "Test program with phase settings yields 54321 in part 1"
  {:mem [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0]
   :phase [0 1 2 3 4]})

(def trial3
  "Test program with phase settings yields 65210 in part 1"
  {:mem [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0]
   :phase [1 0 4 3 2]})

(def trial4
  "Test program and phase data for the feedback loop tests"
  {:mem [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
   :phase [9 8 7 6 5]})

(def trial5
  "Test program and phase data for the feedback loop tests"
  {:mem [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]
   :phase [9 7 8 5 6]})

(defn calc
  "Function to take a program's memory, a series of phase settings, and an
  input, and return the final output of the program after running it with the
  provided number of phase settings, where the output of one run is the input
  of the next."
  [mem phase inp]
  (let [iv (atom inp)]
    (doseq [pi phase]
      (reset! iv (first (:output (run mem [pi @iv])))))
    @iv))

(defn feedback
  "Function to use a feedback loop on all five amplifiers, but to pause at
  any I/O operations, as well as a halt, and to allow the output of one
  stage to be the input of the next, and then loop the output of the last
  to the input of the first, and repeat until there is a halt. This will
  then leave the final output in either the output, or input, of the halted
  amplifier, and we can read it there."
  [mem phase inp]
  (loop [amps [{:name :a :memory mem :input [(nth phase 0)]}
               {:name :b :memory mem :input [(nth phase 1)]}
               {:name :c :memory mem :input [(nth phase 2)]}
               {:name :d :memory mem :input [(nth phase 3)]}
               {:name :e :memory mem :input [(nth phase 4)]}]
         iv inp]
    (let [nu (update (first amps) :input concat [iv])
          ao (merge (run (assoc nu :io-wait true)) (select-keys nu [:name]))]
      (if (= (:state ao) :halt)
        (or (first (:output ao)) (first (:input ao)))
        (recur (concat (rest amps) [(update ao :output rest)]) (first (:output ao)))))))

(defn one
  "Function to find the maximum thrust achievable on this amplifier program
  by trying all the different phase settings, and seeing which achieves the
  best final thrust."
  []
  (->> (for [phs (cmc/permutations [0 1 2 3 4])] (calc puzzle phs 0))
       (sort)
       (last)))

(defn two
  "Function to find the maximum thrust achievable on the feedback configuration
  of this amplifier program by trying all the different phase settings, feeding
  the output of the last into the input of the first, and seeing which achieves
  the best final thrust."
  []
  (->> (for [phs (cmc/permutations [5 6 7 8 9])] (feedback puzzle phs 0))
       (sort)
       (last)))
