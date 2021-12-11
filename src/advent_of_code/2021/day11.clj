(ns advent-of-code.2021.day11
  "Eleventh day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the array of 100 octopi that are in the cave with me
  on the sub, and their energy levels."
  (-> (slurp "resources/2021/input/day11.txt")
    (trim)
    (split #"\n")
    (as-> s (map parse-int (map seq s)))))

(def test1
  "Test data for the first part."
  (-> ["5483143223"
       "2745854711"
       "5264556173"
       "6141336146"
       "6357385478"
       "4167524645"
       "2176841721"
       "6882881134"
       "4846848554"
       "5283751526"]
    (as-> s (map parse-int (map seq s)))))

(defn astep
  "Function to progress the octopi through one step, as they increase in energy
  and then flash, and continue to update those octopi around them. This will
  continue until there are no more flashes, and return the new energy levels
  and the number of octopi flashes for this step."
  [arg]
  (let [rmax (count arg)
        cmax (count (first arg))]
    (loop [brd arg
           delta (repeat rmax (repeat cmax 1))]
      (let [bump (atom [])
            nxt (doall
                  (for [r (range rmax)
                        :let [row (nth brd r)
                              deltar (nth delta r)]]
                    (doall
                      (for [c (range cmax)
                            :let [oct (nth row c)
                                  nxto (+ oct (nth deltar c))]]
                        (do
                          (if (and (<= oct 9) (< 9 nxto))
                            (doseq [br [(dec r) r (inc r)]
                                    bc [(dec c) c (inc c)]
                                    :when (and (not= [r c] [br bc])
                                            (< -1 br rmax) (< -1 bc cmax))]
                              (swap! bump conj [br bc])))
                          (min nxto 10))))))
            cnts (frequencies @bump)]
        (if (empty? @bump)
          (let [ans (for [row nxt] (for [oct row] (if (< 9 oct) 0 oct)))]
            {:board ans :flash (count (filter zero? (apply concat ans)))})
          (recur nxt (for [r (range rmax)] (for [c (range cmax)] (get cnts [r c] 0)))))))))

(defn one
  "Function to find the total number of flashes for the first 100 steps of the
  octopi energy cycling."
  [& [coll]]
  (loop [brd puzzle
         flash 0
         cnt 100]
    (if (<= 1 cnt)
      (let [nxt (astep brd)]
        (recur (:board nxt) (+ flash (:flash nxt)) (dec cnt)))
      {:board brd :flash flash})))

(defn two
  "Function to find the steps it takes to get all the octopi to flash at the
  same time - this is just the reverse and looking for a flash count of 100."
  [& [coll]]
  (loop [brd puzzle
         cnt 1]
    (let [nxt (astep brd)]
      (if (= 100 (:flash nxt))
        (assoc nxt :count cnt)
        (recur (:board nxt) (inc cnt))))))
