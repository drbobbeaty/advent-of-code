(ns advent-of-code.2017.day09
  "Ninth day's solutions for the Advent of Code 2017"
  (require [advent-of-code.util :refer [parse-int sum]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(def puzzle
  "This is the sequence of items in the stream as it flows by."
  (->> (slurp "resources/2017/input/day09.txt")
    (cs/trim)))

(def tests
  ""
  ["<>"
   "<random characters>"
   "<<<<>"
   "<{!>}>"
   "<!!>"
   "<!!!>>"
   "<{o\"i!a,<{i<a>"
   "{}"
   "{{{}}}"
   "{{},{}}"
   "{{{},{},{{}}}}"
   "{<{},{},{{}}>}"
   "{<a>,<a>,<a>,<a>}"
   "{{<a>},{<a>},{<a>},{<a>}}"
   "{{<!>},{<!>},{<!>},{<a>}}"
   "{{<ab>},{<ab>},{<ab>},{<ab>}}"
   "{{<!!>},{<!!>},{<!!>},{<!!>}}"
   "{{<a!>},{<a!>},{<a!>},{<ab>}}"])

(defn run
  "Function to run through the sequence of 'things' in the stream and count
  up the groups, and their value, and return them to the caller."
  [s]
  (loop [sc s
         in-gbg false
         grps []
         dpth 0
         gbg 0]
    (if-let [c (first sc)]
      (cond
        (and (not in-gbg) (= c \<)) (recur (rest sc) true grps dpth gbg)
        (= c \>) (recur (rest sc) false grps dpth gbg)
        (and in-gbg (= c \!)) (recur (drop 2 sc) in-gbg grps dpth gbg)
        in-gbg (recur (rest sc) in-gbg grps dpth (inc gbg))
        (= c \{) (recur (rest sc) in-gbg (conj grps (inc dpth)) (inc dpth) gbg)
        (= c \}) (recur (rest sc) in-gbg grps (dec dpth) gbg)
        :else (recur (rest sc) in-gbg grps dpth gbg))
      {:garbage gbg :in-garbage? in-gbg :groups grps :depth dpth :score (apply + grps)})))

(defn one
  "Function to take the stream of 'things' in the stream that were recorded,
  and runs it through the processing code to count the groups and the number
  of characters of garbage."
  []
  (run puzzle))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (for [ts tests]
    (run ts)))
