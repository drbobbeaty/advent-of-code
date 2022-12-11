(ns advent-of-code.2022.day11
  "Eleventh day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the starting state of the monkeys."
  {:monkey0 {:items (atom [96, 60, 68, 91, 83, 57, 85])
             :cnt (atom 0)
             :inspect (fn [x] (* x 2))
             :test-val 17
             :test (fn [x] (if (zero? (mod x 17)) :monkey2 :monkey5))}
   :monkey1 {:items (atom [75, 78, 68, 81, 73, 99])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 3))
             :test-val 13
             :test (fn [x] (if (zero? (mod x 13)) :monkey7 :monkey4))}
   :monkey2 {:items (atom [69, 86, 67, 55, 96, 69, 94, 85])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 6))
             :test-val 19
             :test (fn [x] (if (zero? (mod x 19)) :monkey6 :monkey5))}
   :monkey3 {:items (atom [88, 75, 74, 98, 80])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 5))
             :test-val 7
             :test (fn [x] (if (zero? (mod x 7)) :monkey7 :monkey1))}
   :monkey4 {:items (atom [82])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 8))
             :test-val 11
             :test (fn [x] (if (zero? (mod x 11)) :monkey0 :monkey2))}
   :monkey5 {:items (atom [72, 92, 92])
             :cnt (atom 0)
             :inspect (fn [x] (* x 5))
             :test-val 3
             :test (fn [x] (if (zero? (mod x 3)) :monkey6 :monkey3))}
   :monkey6 {:items (atom [74, 61])
             :cnt (atom 0)
             :inspect (fn [x] (* x x))
             :test-val 2
             :test (fn [x] (if (zero? (mod x 2)) :monkey3 :monkey1))}
   :monkey7 {:items (atom [76, 86, 83, 55])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 4))
             :test-val 5
             :test (fn [x] (if (zero? (mod x 5)) :monkey4 :monkey0))}})

(def test1
  "Test data for the first part."
  {:monkey0 {:items (atom [79 98])
             :cnt (atom 0)
             :inspect (fn [x] (* x 19))
             :test-val 23
             :test (fn [x] (if (zero? (mod x 23)) :monkey2 :monkey3))}
   :monkey1 {:items (atom [54 65 75 74])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 6))
             :test-val 19
             :test (fn [x] (if (zero? (mod x 19)) :monkey2 :monkey0))}
   :monkey2 {:items (atom [79 60 97])
             :cnt (atom 0)
             :inspect (fn [x] (* x x))
             :test-val 13
             :test (fn [x] (if (zero? (mod x 13)) :monkey1 :monkey3))}
   :monkey3 {:items (atom [74])
             :cnt (atom 0)
             :inspect (fn [x] (+ x 3))
             :test-val 17
             :test (fn [x] (if (zero? (mod x 17)) :monkey0 :monkey1))}})

(defn round
  "Function to update the 'chimps' for one round of their game. If the
  'relief' is true, then we will get the relief in part 1, but if not,
  as in part 2, then no relief at all."
  [chimps max-worry]
  (doseq [m (range (count chimps))
          :let [k (keyword (str "monkey" m))
                itms @(:items (k chimps))
                _ (reset! (:items (k chimps)) [])]
          i itms
          :let [insp ((:inspect (k chimps)) i)
                bored (if max-worry (mod insp max-worry) (quot insp 3))
                tgt ((:test (k chimps)) bored)]]
    (swap! (:items (tgt chimps)) conj bored)
    (swap! (:cnt (k chimps)) inc)))

(defn one
  "Function to find the state of the chimps after 20 rounds of their little
  game, and then find the two most active chimps."
  [& [coll]]
  (let [chimps puzzle]
    (doseq [r (range 20)]
      (round chimps false))
    ;; now compute the two most active chimps
    (->> (vals chimps)
      (map :cnt)
      (map deref)
      (sort)
      (take-last 2)
      (apply *))))

(defn two
  "Function to find the state of the chimps after 10000 rounds of their
  little game, without the relaxing, but rolling over at the multiple of
  all test values, and then find the two most active chimps."
  [& [coll]]
  (let [chimps puzzle
        max-worry (apply * (map :test-val (vals chimps)))]
    (doseq [r (range 10000)]
      (round chimps max-worry))
    ;; now compute the two most active chimps
    (->> (vals chimps)
      (map :cnt)
      (map deref)
      (sort)
      (take-last 2)
      (apply *))))
