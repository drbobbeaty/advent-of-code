(ns advent-of-code.2016.day07
  "Fourth day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(def puzzle
  "This is the input from the code to the Easter Bunny's directory. The result
  of this will be a sequence of room maps."
  (-> (slurp "resources/2016/input/day07.txt")
      (cs/trim)
      (cs/split #"\n")))

(defn abba?
  "Function to see if the IP address supports TLS via the ABBA check."
  [ipa]
  (let [rev? (fn [[a b c d]] (and (= a d) (= b c) (not= a b)))
        hit? (fn [s] (some rev? (partition 4 1 s)))
        pts (partition-all 2 (cs/split ipa #"\[|\]"))]
    (and (some hit? (map first pts)) (every? #(not (hit? %)) (map second pts)))))

(defn aba?
  "Function to see if the IP address supports TLS via the ABBA check."
  [ipa]
  (let [ab? (fn [[a b c]] (and (= a c) (not= a b)))
        hit (fn [s] (->> (filter ab? (partition 3 1 s))
                         (map #(apply str %))))
        flip (fn [s] (apply str [(second s) (first s) (second s)]))
        pts (partition-all 2 (cs/split ipa #"\[|\]"))
        tst (->> (map first pts)
                 (map hit)
                 (apply concat)
                 (map flip)
                 (distinct))]
      (some identity (for [t tst
                           h (map second pts)
                           :when (and t h)]
                       (cs/includes? h t)))))

(defn one
  "Count the number of IP addresses that meet the ABBA criteria."
  [ips]
  (count (filter abba? ips)))

(defn two
  "Count the number of IP addresses that meet the ABA criteria."
  [ips]
  (count (filter aba? ips)))
