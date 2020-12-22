(ns advent-of-code.2020.day21
  "Twenty-first day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split un-seq]]
            [clojure.set :refer [intersection]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to parse a line of the ingredients from the puzzle input."
  [s]
  (let [[ing bad] (rest (re-matches #"^(.+) \(contains (.+)\)$" s))]
    {:ingredients (split (trim ing) " ") :bad (split (trim bad) ", ")}))

(def puzzle
  "This is the input of the ingredients and allergens that we need to be
  careful about."
  (-> (slurp "resources/2020/input/day21.txt")
      (trim)
      (split #"\n")
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
       "trh fvjkl sbzzf mxmxvkd (contains dairy)"
       "sqjhc fvjkl (contains soy)"
       "sqjhc mxmxvkd sbzzf (contains fish)"]
      (->> (map parse))))

(defn unravel
  "Function to look at the menu items, with the allergens, and then see what
  I can remove from the mapping. At that point, I remove it from the bad and
  ingredient sides, and add it to the map, and return that. We will call this
  over and over, and come up with a complete mapping of the allergens."
  [coll ans]
  (let [bs (map first
             (reverse
               (sort-by second
                 (for [a (partition-by identity (sort (apply concat (map :bad coll))))]
                   [(first a) (count a)]))))]
    (loop [bis bs]
      (if-let [bad (first bis)]
        (let [is (->> (filter #(<= 0 (.indexOf (:bad %) bad)) coll)
                      (map :ingredients)
                      (map set)
                      (apply intersection))]
          (if (= 1 (count is))
            [(for [{b :bad i :ingredients} coll]
               {:bad (remove #(= bad %) b) :ingredients (remove #(= (first is) %) i)})
             (assoc ans bad (first is))]
            (recur (rest bis))))))))

(defn one
  "Function to count the inert ingredients by coming up with the mapping, and
  then removing those ingredients from the total list of all ingredients."
  [& [coll]]
  (let [menu (or coll puzzle)
        aller (loop [mis menu
                     ans {}]
                (if (< 0 (count (filter #(not-empty (:bad %)) mis)))
                  (let [[c m] (unravel mis ans)]
                    (recur c m))
                  ans))
        ings (->> (map :ingredients menu)
                  (apply concat)
                  (remove (set (vals aller))))]
    [aller ings (count ings)]))

(defn two
  "Function to take the mapping of allergens to ingredients, sort and join
  them to make the answer to the second part."
  [& [coll]]
  (let [[aller _ _] (one (or coll puzzle))]
    (->> (seq aller)
         (sort-by first)
         (map second)
         (cs/join ","))))
