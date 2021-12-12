(ns advent-of-code.2021.day12
  "Twelfth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to parse the lines of text to make a map of possible next caves
  to visit from the current cave location."
  [coll]
  (let [paths (map #(split % #"-") coll)]
    (into {}
      (for [c (distinct (apply concat paths))]
        [c (->> (filter #(<= 0 (.indexOf % c)) paths)
                  (map (fn [cvs] (remove #(= c %) cvs)))
                  (apply concat))]))))

(def puzzle
  "This is the input of the connectivity, and size of the caves in the puzzle,
  we take these as pairs, and map them into a useful topology."
  (-> (slurp "resources/2021/input/day12.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["start-A"
       "start-b"
       "A-c"
       "A-b"
       "b-d"
       "A-end"
       "b-end"]
    (parse)))

(def test2
  "This is more test data from the puzzle statement"
  (-> (slurp "resources/2021/input/day12-test2.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test3
  "This is more test data from the puzzle statement"
  (-> (slurp "resources/2021/input/day12-test3.txt")
    (trim)
    (split #"\n")
    (parse)))

(defn safe?
  "Predicate function to see if the provided cave is OK to go to, given the
  current visited caves in the path. This will allow large caves to be visited
  twice, and small caves to be visited only once."
  [path cave]
  (or (neg? (.indexOf path cave)) (re-matches #"[A-Z]+" cave)))

(defn walk
  "Function to take a topology, location, and current path, and return a
  sequence of paths from here to the end based on the possible paths through
  the topology."
  [topo sfn loc path]
  (if (= loc "end")
    [(conj path loc)]
    (apply concat
      (for [nc (filter #(sfn path %) (get topo loc []))]
        (walk topo sfn nc (conj path loc))))))

(defn one
  "Function to find the total number of paths through the cave system where
  we can visit the large caves many times, but the small caves only once."
  [& [coll]]
  (let [paths (walk puzzle safe? "start" [])]
    {:paths paths :count (count paths)}))

(defn two
  "Function to find the total number of paths through the cave system where
  each one of the small caves, in turn, can be visited twice - but that's it,
  and then we have to sum them all up."
  [& [coll]]
  (let [topo puzzle
        sms (->> (keys topo)
              (remove #(or (= "start" %) (= "end" %)))
              (remove #(re-matches #"[A-Z]+" %)))
        paths (atom [])]
    (doseq [c sms
            :let [dvsf (fn [path cave]
                         (or (neg? (.indexOf path cave))
                           (re-matches #"[A-Z]+" cave)
                           (and (= c cave) (<= (count (filter #(= cave %) path)) 1))))]]
      (reset! paths (distinct (concat @paths (walk topo dvsf "start" [])))))
    {:paths @paths :count (count @paths)}))
