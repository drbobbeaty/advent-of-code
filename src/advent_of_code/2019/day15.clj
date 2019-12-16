(ns advent-of-code.2019.day15
  "Fifteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program for the repair droid."
  (-> (slurp "resources/2019/input/day13.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn one
  ""
  [mem]
  (let [dir-chg [[0 1] [0 -1] [-1 0] [1 0]]  ;; n s w e
       ]
  (loop [cpu (if (map? mem) mem {:memory mem})
         dp [0 0]
         brd []
         dd 1
        ]
    (let [ncpu (run (assoc cpu :input [dd] :output [] :io-wait true))
          stat (first (:output ncpu))
          tpos (mapv sum (map vector dp (nth dir-chg (dec dd))))
         ]
      (case stat
        0 (conj brd [tpos 0])
        1
        2
      )
    )
  )
  )
