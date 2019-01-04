(ns advent-of-code.2017.day24
  "Twenty-fourth day's solution for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int sum split rev]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the list of components that the puzzle has."
  (-> (slurp "resources/2017/input/day24.txt")
    (cs/trim)
    (split "\n")
    (as-> s (map (fn [p] (mapv parse-int (split p "/"))) s))))

(def sample
  "This is the sample list of components used in the puzzle."
  (-> ["0/2"
       "2/2"
       "2/3"
       "3/4"
       "3/5"
       "0/1"
       "10/1"
       "9/10"]
    (as-> s (map (fn [p] (mapv parse-int (split p "/"))) s))))

(defn ports
  "Function to take a socket type (a number) and returns a sequence of
  bridge segments from the list of all available bridge segments. This
  is the way to select the 'next possible' bridge segment to put on the
  end of the bridge."
  [n ps]
  (not-empty (filter #(or (= n (first %)) (= n (second %))) ps)))

(defn other
  "Function to find the 'other' end of the bridge segment based on the
  number that was used to extract this bridge segment from the list of
  all possible segments to add."
  [a [b c]]
  (if (= a b) c b))

(defn bridge
  "Function to expand all possible bridges starting with the number
  supplied, and the list of possible bridge segments supplied. This
  will loop until all possible bridges have been built - even if they
  are a sub-bridge of a longer bridge."
  [n pbs]
  (loop [cbs []
         lbs (for [bc (ports n pbs)]
               {:bridge [bc]
                :rest (remove #(= bc %) pbs)
                :nxt (other n bc)})]
    (let [nxt (for [{brs :bridge ps :rest n :nxt :as bst} lbs
                    bc (ports n ps)]
                (if bc
                  {:bridge (conj brs bc)
                   :rest (remove #(= bc %) ps)
                   :nxt (other n bc)}))]
      (if (pos? (count nxt))
        (recur (concat cbs lbs) nxt)
        (map :bridge (concat cbs lbs nxt))))))

(defn one
  "Function to find the strongest bridge given the puzzle sequence of
  bridge elements - starting at a socket type of 0."
  []
  (->> (for [br (bridge 0 puzzle)
             :let [bs (sum (flatten br))]]
         {:bridge br :strength bs})
    (sort-by :strength >)
    (first)))

(defn two
  "Function to return the longest, strongest bridge we can make. This is
  taking the same kind of sequence, but sorting it in a different way."
  []
  (->> (for [br (bridge 0 puzzle)
             :let [bs (sum (flatten br))
                   bl (count br)]]
         {:bridge br :strength bs :length bl})
    (sort-by (juxt :length :strength) rev)
    (first)))

(defn yoyo
  "Function to test the components as we are building up things. That's it."
  []
  (->> (for [br (bridge 0 sample)
             :let [bs (sum (flatten br))
                   bl (count br)]]
         {:bridge br :strength bs :length bl})
    (sort-by (juxt :length :strength) rev)
    (first)))
