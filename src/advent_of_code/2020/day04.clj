(ns advent-of-code.2020.day04
  "Fourth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]))

(defn collect
  "Function to collect the source data into one line per passport. This is
  key to the parsing of the data, as it's far easier when it's all in one
  line, and then we process that directly."
  [s]
  (loop [src s
         all (transient [])
         lst ""]
    (if-let [f (first src)]
      (let [[na nl] (if (pos? (count f))
                      [all             (trim (str lst " " f))]
                      [(conj! all lst) f])]
        (recur (rest src) na nl))
      (persistent! (conj! all lst)))))


(defn parse
  "Function to take a string of passport data and turn it into a map where
  the keys are the fields and the values are the parsed values."
  [s]
  (into {}
    (for [l (split s " ")
          :let [[k v] (split l ":")]]
      [(keyword k) v])))

(def puzzle
  "This is the input the pattern of the trees on the way to the airport."
  (-> (slurp "resources/2020/input/day04.txt")
      (trim)
      (split #"\n")
      (collect)
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
       "byr:1937 iyr:2017 cid:147 hgt:183cm"
       ""
       "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
       "hcl:#cfa07d byr:1929"
       ""
       "hcl:#ae17e1 iyr:2013"
       "eyr:2024"
       "ecl:brn pid:760753108 byr:1931"
       "hgt:179cm"
       ""
       "hcl:#cfa07d eyr:2025 pid:166559648"
       "iyr:2011 ecl:brn hgt:59in"]
      (collect)
      (->> (map parse))))

(defn decent?
  "Predicate function to see if the passport data, as a map, is good enough
  for the folks at passport control. If it's missing :cid, that's OK, but it
  can't miss anything else."
  [m]
  (let [cnt (count (vals m))]
    (or (= 8 cnt)
        (and (= 7 cnt) (not (contains? m :cid))))))

(defn valid?
  "Predicate function to look and see not only if the passport data is
  _reasonable_, but if it matches the data validation checks that are
  needed to get me through the gates..."
  [m]
  (and (decent? m)
    (every? boolean
      (for [[k v] m]
        (case k
          :byr (<= 1920 (parse-int v) 2002)
          :iyr (<= 2010 (parse-int v) 2020)
          :eyr (<= 2020 (parse-int v) 2030)
          :hgt (cond
                 (.endsWith v "in") (<= 59 (parse-int (subs v 0 (- (count v) 2))) 76)
                 (.endsWith v "cm") (<= 150 (parse-int (subs v 0 (- (count v) 2))) 193))
          :hcl (and (string? v) (re-matches #"^\#[0-9a-f]{6}$" v))
          :ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)
          :pid (and (string? v) (re-matches #"^[0-9]{9}$" v))
          :cid true
          false)))))

(defn one
  "Function to count the decent, but not value-checked passports in the
  input data set."
  [& [coll]]
  (count (filter identity (map decent? (or coll puzzle)))))

(defn two
  "Function to count the valid data passports in the data set."
  [& [coll]]
  (count (filter identity (map valid? (or coll puzzle)))))
