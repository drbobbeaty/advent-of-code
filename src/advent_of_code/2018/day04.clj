(ns advent-of-code.2018.day04
  "Fourth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [java-time :refer [as local-date-time]]
            [clojure.string :as cs]))

(def puzzle
  "This is the source list of all elf fabric claims."
  (-> "resources/2018/input/day04.txt"
      (io/reader)
      (line-seq)))

(def ^:private elf-fmt "yyyy-MM-dd HH:mm")

(defn first-pass
  "Function to do the simple parsing of the input from the guard schedule to
  something that can be sorted, and then used to compute the attack plan."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))
        pdte (fn [x] (local-date-time elf-fmt (cs/trim x)))]
    (cond
      (string? s) (let [rts (drop 1 (re-matches #"^\[(.+)\] .*$" s))
                        ts (pdte (first rts))
                        base {:time ts
                              :date (format "%02d-%02d" (as ts :month-of-year) (as ts :day-of-month))
                              :mins (as ts :minute-of-hour)}]
                    (cond
                      (.endsWith s "begins shift")
                        (let [pts (drop 1 (re-matches #"^\[.+\] Guard #(\d+) begins shift$" s))]
                          (assoc base :id (pint (first pts)) :state :on-duty))
                      (.endsWith s "falls asleep")
                        (assoc base :state :asleep)
                      (.endsWith s "wakes up")
                        (assoc base :state :awake)))
      (coll? s)   (sort-by :time (map first-pass s))
      :else       s)))

(defn code-it
  "Function to take the puzzle input and turn it into a series of
  records of the sleeping of a single guard on a single day:
    {:id 99
     :date \"11-01\"
     :sleep [5 25]}
  and this sequence will be much easier to process. It's important to note
  that the `:sleep` interval is open-ended on the high end, and that minute
  should *not* be included."
  [s]
  (let [out (atom [])
        guard (atom 0)
        sleep (atom 0)
        shift (atom nil)]
    (doseq [r (first-pass s)]
      (case (:state r)
        :on-duty
          (reset! guard (:id r))
        :asleep
          (do
            (reset! sleep (:mins r))
            (reset! shift (:date r)))
        :awake
          (swap! out conj {:id @guard :date @shift :sleep [@sleep (:mins r)]})))
    @out))

(defn total-sleep
  "Function to take a sequence of maps of the form:
    {:id 99
     :date \"11-01\"
     :sleep [5 25]}
  and computes the total time slept by each guard for the entire duration of
  the sequence."
  [s]
  (let [out (atom {})
        add (fn [a b] (+ (or a 0) (or b 0)))]
    (doseq [r s]
      (swap! out update (:id r) add (- (apply - (:sleep r)))))
    (->> (for [[k v] @out]
          {:id k :sleep v})
      (sort-by :sleep >))))

(defn most-slept
  "Function to take a sequence of maps of the form:
    {:id 99
     :date \"11-01\"
     :sleep [5 25]}
  and computes the most often slept minute for each of the guards in the
  source sequence. The return value is a sequence of maps, one per guard
  of the form:
    {:id 99
     :most 45
     :cnt 3}
  meaning that Guard #99 was asleep the most in minute 45 - a total of 3
  times."
  [s]
  (for [id (set (map :id s))
        :let [ss (->> (filter #(= id (:id %)) s)
                   (map #(apply range (:sleep %)))
                   (flatten)
                   (frequencies)
                   (sort-by val >)
                   (first))]]
    {:id id :most (first ss) :cnt (second ss)}))

(defn one
  "Function to find the guard that sleeps the most, and the minute that he is
  most often found to be asleep. This is all based on the input to this
  function."
  [& [src]]
  (let [slp (code-it (or src puzzle))
        big (first (total-sleep slp))]
    (-> (filter #(= (:id big) (:id %)) (most-slept slp))
      (first)
      (merge big))))

(defn two
  "Function to find the guard that is asleep the most for a given minute, and
  report on him."
  [& [src]]
  (let [slp (code-it (or src puzzle))]
    (first (sort-by :cnt > (most-slept slp)))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src (code-it ["[1518-11-01 00:00] Guard #10 begins shift"
                      "[1518-11-01 00:05] falls asleep"
                      "[1518-11-01 00:25] wakes up"
                      "[1518-11-01 00:30] falls asleep"
                      "[1518-11-01 00:55] wakes up"
                      "[1518-11-01 23:58] Guard #99 begins shift"
                      "[1518-11-02 00:40] falls asleep"
                      "[1518-11-02 00:50] wakes up"
                      "[1518-11-03 00:05] Guard #10 begins shift"
                      "[1518-11-03 00:24] falls asleep"
                      "[1518-11-03 00:29] wakes up"
                      "[1518-11-04 00:02] Guard #99 begins shift"
                      "[1518-11-04 00:36] falls asleep"
                      "[1518-11-04 00:46] wakes up"
                      "[1518-11-05 00:03] Guard #99 begins shift"
                      "[1518-11-05 00:45] falls asleep"
                      "[1518-11-05 00:55] wakes up"])
        big (first (total-sleep src))
        hits (->> (filter #(= (:id big) (:id %)) src)
               (map #(apply range (:sleep %)))
               (flatten)
               (frequencies)
               (sort-by val >)
               (first))]
    (assoc big :most (first hits) :cnt (second hits))))
