(ns advent-of-code.2020.day07
  "Seventh day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]))

(defn parse
  "Function to parse a line of the bags rule into something that's a more
  reasonable data structure to deal with."
  [s]
  (let [[clr mix] (rest (re-matches #"^(.*) bags contain (.*)\.$" s))
        itm (fn [s] (let [[c b] (rest (re-matches #"^(\d+) (.+) bags?$" s))]
                      {:color b :cnt (parse-int c)}))]
    {:color clr
     :holds (if-not (.startsWith mix "no other")
              (map itm (split mix ", ")))}))

(def puzzle
  "This is the input of the rules for what bags contain what bags. It's
  all being processed by 'parse' to make it far more usable to the rest of
  the code."
  (-> (slurp "resources/2020/input/day07.txt")
      (trim)
      (split #"\n")
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["light red bags contain 1 bright white bag, 2 muted yellow bags."
       "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
       "bright white bags contain 1 shiny gold bag."
       "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
       "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
       "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
       "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
       "faded blue bags contain no other bags."
       "dotted black bags contain no other bags."]
      (->> (map parse))))

(def test2
  "Test data for the second part."
  (-> ["shiny gold bags contain 2 dark red bags."
       "dark red bags contain 2 dark orange bags."
       "dark orange bags contain 2 dark yellow bags."
       "dark yellow bags contain 2 dark green bags."
       "dark green bags contain 2 dark blue bags."
       "dark blue bags contain 2 dark violet bags."
       "dark violet bags contain no other bags."]
      (->> (map parse))))

(defn can-hold
  "Function to take a collection of rules and a bag color and return all the
  bags that can *directly* hold that bag. No indirection here."
  [coll bag]
  (-> (for [{clr :color hld :holds} coll
            :let [hit (first (filter #(= bag (:color %)) hld))]
            :when hit]
        clr)
      (distinct)))

(defn one
  "Function to find the total number of bags that can contain an 'shink gold'
  bag - either directly or indirectly. This is a simple bottom-up search where
  we look for what can hold the 'shiny gold' bag, and then what can hold those
  and so on until we get to the top of the tree."
  [& [coll]]
  (let [rules (or coll puzzle)]
    (loop [srch ["shiny gold"]
           ans (transient #{})]
      (let [nxt (distinct (flatten (map #(can-hold rules %) srch)))]
        (if (not-empty nxt)
          (recur nxt (reduce conj! ans nxt))
          (count (persistent! ans)))))))

(defn holds
  "Function to recursively count how many bags are in a given colored bag,
  and then return that to the caller. It will return a sequence of all the
  counts for all the possible colors in the bag, and you just have to sum
  all those to get a total count."
  [coll bag]
  (let [me (first (filter #(= bag (:color %)) coll))]
    (for [{sz :cnt cl :color :as h} (:holds me)
          :let [ins (holds coll cl)]]
      (if (empty? ins)
        sz
        (+ sz (* sz (sum ins)))))))

(defn two
  "Function to count all the bags that can fit into my one 'shiny gold'
  bag, and then return that."
  [& [coll]]
  (let [rules (or coll puzzle)]
    (sum (holds rules "shiny gold"))))
