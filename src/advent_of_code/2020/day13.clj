(ns advent-of-code.2020.day13
  "Thirteenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]
            [clojure.math.numeric-tower :refer [ceil]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the ."
  (let [[arr sched] (-> (slurp "resources/2020/input/day13.txt")
                        (trim)
                        (split #"\n"))]
    {:start (parse-int arr) :buses (map parse-int (split sched ","))}))

(def test1
  "Test data for the first part."
  {:start 939
   :buses (map parse-int (split "7,13,x,x,59,x,31,19" ","))})

(defn one
  "Function to find the soonest bus we can catch to the airport, given they
  are on their own schedule."
  [& [m]]
  (let [{st :start bs :buses :as arg} (or m puzzle)]
    (->> (for [b bs
               :when (pos? b)
               :let [miss (mod st b)
                     c (int (ceil (/ st b)))
                     w (- (* c b) st)]]
           {:miss miss :bus b :wait w :id (* b w)})
         (sort-by :wait)
         (first))))

(defn two
  "Function to find when they are all lined up based on their spacing in the
  schedule. This was a problem I needed help on, and Bret's solution was
  quite elegant, and I need to do a lot more reading on this subject."
  [& [m]]
  (let [{st :start bs :buses :as arg} (or m puzzle)
        buses (->> (map vector (range) bs)
                   (remove #(zero? (second %))))]
    (loop [src buses
           lcm 1
           time 0]
      (if (<= 2 (count src))
        (let [[os bf] (first src)
              [nos nbf] (second src)
              nlcm (* lcm bf)
              ntime (loop [t time]
                      (if (pos? (mod (+ t nos) nbf))
                        (recur (+ t nlcm))
                        t))]
          (recur (rest src) nlcm ntime))
        {:lcm lcm :time time}))))
