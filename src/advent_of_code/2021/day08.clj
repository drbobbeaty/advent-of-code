(ns advent-of-code.2021.day08
  "Eighth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median mean]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a line of digits and output values and put it into a more
  reasonable data structure. This includes ordering the characters in the
  individual digits as order doesn't matter, and matching will be far easier
  this way."
  [s]
  (let [chop (fn [a] (-> (trim a)
                       (split #" ")
                       (->> (map #(apply str (sort %))))))
        [ds ov] (map chop (split (trim s) #"\|"))]
    {:digits ds :output ov}))

(def puzzle
  "This is the input of the digit sequences and output codes for the sub display"
  (-> (slurp "resources/2021/input/day08.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
       "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
       "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
       "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
       "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
       "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
       "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
       "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
       "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
       "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]
    (->> (map parse))))

(defn one
  "Function to find the number of 1, 4, 7, 8s that appear in the output values
  of the input based on the fact that those are easily determined becuase they
  are each a unique length."
  [& [coll]]
  (let [cnts (->> (map :output puzzle)
               (apply concat)
               (map count)
               (frequencies))
        one (get cnts 2)
        four (get cnts 4)
        seven (get cnts 3)
        eight (get cnts 7)]
    {:one one :four four :seven seven :eight eight :sum (+ one four seven eight)}))

(defn within?
  "Predicate function to return 'true' of all the characters in the substring
  'ss' are in the taget string 'tgt' - even if they aren't consecutive."
  [tgt ss]
  (let [arr (seq tgt)]
    (every? #(<= 0 (.indexOf arr %)) ss)))

(defn decode
  "Function to take a map of the garbled seven-segmment codes, and determine
  which is which digit by looking at the consistent representation of the
  data across all ten digits. It's really just simple pattern matching."
  [coll]
  (let [zap (fn [f] (first (filter f coll)))
        one (zap #(= 2 (count %)))
        four (zap #(= 4 (count %)))
        seven (zap #(= 3 (count %)))
        eight (zap #(= 7 (count %)))
        three (zap #(and (= 5 (count %)) (within? % one)))
        nine (zap #(and (= 6 (count %)) (within? % three)))
        six (zap #(and (= 6 (count %)) (not (within? % one))))
        five (zap #(and (= 5 (count %)) (within? six %)))
        zero (zap #(and (= 6 (count %)) (not= % six) (not= % nine)))
        two (zap #(and (= 5 (count %)) (not= % three) (not= % five)))]
    {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}))

(defn two
  "Function to find the sum of all the output values of all the decoded digits
  for each row. This just decodes a row, then computes the output, and adds
  them all up."
  [& [coll]]
  (let [lines (for [{dig :digits out :output} puzzle
                   :let [ref (decode dig)]]
                [out (parse-int (apply str (map #(get ref %) out)))])]
    {:lines lines :sum (sum (map second lines))}))
