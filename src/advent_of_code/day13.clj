(ns advent-of-code.day13
  (:require [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the seating chart."
  (into {} (for [[k v] (group-by first
                         (for [l (cs/split-lines (slurp "resources/input/day13.txt"))
                              :let [[_ per dir pts nei] (re-matches #"(.*?) would (.*?) (.*?) happiness units by sitting next to (.*?)." l)]]
                           [per (* (if (= "gain" dir) 1 -1) (Integer/parseInt pts)) nei]))]
             [k (into {} (for [[_ pts nei] v] [nei pts]))])))

(def ^:dynamic *network* nil)

(defn price
  "Function to 'price' the happiness for the provided seating shart - all sitting
  in a circle."
  [sa]
  (let [sc (-> (partition 3 1 sa)
               (conj (concat [(last sa)] (take 2 sa)))
               (conj (concat (drop (- (count sa) 2) sa) [(first sa)])))]
    (apply + (for [[l m r] sc
                   :let [mm (get *network* m {})
                         lp (get mm l 0)
                         rp (get mm r 0)]]
               (+ lp rp)))))

(defn optimal
  "Function to find the optimal seating arrangement."
  [cs]
  (apply max (map price (permutations cs))))

(defmacro with-puzzle
  "Macro to set up the dynamic network variable for the processing."
  [puz & body]
  `(binding [*network* ~puz]
     ~@body))

(defn part1
  "Function to complete part 1"
  []
  (with-puzzle puzzle
    (optimal (keys puzzle))))

(defn part2
  "Function to complete part 2"
  []
  (let [wme (-> (into {} (for [[k v] puzzle] [k (assoc v "me" 0)]))
                (assoc "me" (into {} (for [p (keys puzzle)] [p 0]))))]
    (with-puzzle wme
      (optimal (keys wme)))))
