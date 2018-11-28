(ns advent-of-code.2017.day01
  "First day's solutions for the Advent of Code 2017"
  (require [clojure.string :as cs]))

(def puzzle
  "This is the input from the path to the Easter Bunny HQ. The result of this
  will be a sequence of tuples of the form: ['R' 3] - a turn and a distance."
  (-> (slurp "resources/2016/input/day01.txt")
      (cs/trim)
      (cs/split #"\, ")
      (->> (map (fn [s] [(subs s 0 1) (Integer/parseInt (subs s 1))])))))

(defn sum-match
  ""
  [x]
  (let [raw (str x)
        src (drop-last 1 (partition-all 2 1 (str raw (first raw))))
       ]
    src

       ))
