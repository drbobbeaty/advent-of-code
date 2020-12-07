(ns advent-of-code.2020.day02
  "Second day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int xor trim split]]))

(defn parse
  "Function to take a raw string of the input from the shopkeeper about the
  passwords."
  [l]
  (let [[lc hc c pw] (drop 1 (re-matches #"^(\d+)-(\d+) ([a-z]): (.+)$" l))]
    {:low (parse-int lc)
     :high (parse-int hc)
     :char (first c)
     :pass pw}))

(def puzzle
  "This is the input of the masses of the modules for the ship."
  (-> (slurp "resources/2020/input/day02.txt")
      (trim)
      (split #"\n")
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (->> ["1-3 a: abcde"
        "1-3 b: cdefg"
        "2-9 c: ccccccccc"]
       (map parse)))

(defn counts?
  "Predicate function to check a parsed password policy and password map. This
  will deconstruct the values of the map, and then test them to see if indeed
  this password adheres to the provided policy. This is using the 'counts'
  rules where ':low' and ':high' are counts of the ':char'."
  [{lc :low hc :high c :char pw :pass :as arg}]
  (and (string? pw) (char? c) (number? lc) (number? hc)
       (<= lc (count (filter #(= c %) pw)) hc)))

(defn loc?
  "Predicate function to check a parsed password policy and password map. This
  will deconstruct the values of the map, and then test them to see if indeed
  this password adheres to the provided policy. This is using the 'location'
  rules where ':low' and ':high' are possible locations of the ':char'."
  [{lc :low hc :high c :char pw :pass :as arg}]
  (and (string? pw) (char? c) (number? lc) (number? hc)
       (xor (= c (.charAt pw (dec lc))) (= c (.charAt pw (dec hc))))))

(defn one
  "Function to find how many of the provided password rules and words are
  valid - using the 'counts?' predicate function."
  [& [coll]]
  (count (filter identity (map counts? (or coll puzzle)))))

(defn two
  "Function to find how many of the provided password rules and words are
  valid - using the 'loc?' predicate function."
  [& [coll]]
  (count (filter identity (map loc? (or coll puzzle)))))
