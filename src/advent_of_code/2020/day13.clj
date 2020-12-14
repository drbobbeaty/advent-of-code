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
  "Function to "
  [& [m]]
  (let [{st :start bs :buses :as arg} (or m test1)
        [[os bf] & mre] (->> (map vector (range) bs)
                             (remove #(zero? (second %)))
                             (sort-by second)
                             (reverse))
        hit? (fn [[os f] t] (= 0 (mod (+ os t) f)))
       ]
    (first
      (for [i (map inc (range))
            :let [t (- (* i bf) os)]
            :when (every? #(hit? % t) mre)
           ]
        t
      )
    )
  )
  ; (first
  ;   (for [i (range 100 5000)
  ;         :when (= 0 (mod i 17) (mod (+ 2 i) 13) (mod (+ 3 i) 19))]
  ;     i))
  ; (first
  ;   (for [i (map inc (range))
  ;         :let [t (- (* i 59) 4)]
  ;         :when (and (= 0 (mod (+ 6 t) 31)) (= 0 (mod (+ t 7) 19)) (= 0 (mod (+ 1 t) 13)) (= 0 (mod t 7)) )
  ;        ]
  ;     t
  ;   )
  ; )
  )
