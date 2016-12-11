(ns advent-of-code.2016.day04
  "Fourth day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(defn ->room
  "Function to convert the input from the Easter Bunny HQ directory into a simple
  map structure that we can more easily work with."
  [arg]
  (cond
    (string? arg) (let [[n id cs] (drop 1 (re-matches #"(.*)-([0-9]+)\[(.*)\]" arg))]
                    {:name n, :id (Integer/parseInt id), :checksum cs})
    (coll? arg)   (map ->room arg)
    :else         arg))

(defn- csum-ord
  "Comparator function to look at the character count and alphabetical listing
  of the characters in the room name, and sort the characters properly for the
  checksum."
  [[av ac] [bv bc]]
  (if (= av bv)
    (< (int ac) (int bc))
    (> av bv)))

(defn valid?
  "Predicate function to test if the room description argument is, in fact, a
  correct room description."
  [{n :name id :id cs :checksum}]
  (let [ccs (->> (remove #(= \- %) n)
                 (frequencies)
                 (sort-by (juxt second first) csum-ord)
                 (take 5)
                 (map first)
                 (apply str))]
    (= cs ccs)))

(def puzzle
  "This is the input from the code to the Easter Bunny's directory. The result
  of this will be a sequence of room maps."
  (-> (slurp "resources/2016/input/day04.txt")
      (cs/trim)
      (cs/split #"\n")
      (->room)))

(defn one
  "Function to take a series of room definitions and then see which ones are
  valid, and finally sum the `:id` values of those valid rooms."
  [rs]
  (->> (filter valid? rs)
       (map :id)
       (apply +)))

(defn decrypt
  "Function to decrypt the name of the room based on the existing name and the
  `:id` value."
  [{n :name id :id cs :checksum}]
  (let [ac (int \a)
        rot (fn [c] (char (+ (mod (+ (- (int c) ac) id) 26) ac)))]
    (apply str (for [c n] (if (= \- c) " " (rot c))))))

(defn two
  "Function to find the room where 'northpole' is in the decrypted name. We
  need to have the `:id`, and that's our answer."
  [rs]
  (filter #(cs/includes? (decrypt %) "northpole") rs))
