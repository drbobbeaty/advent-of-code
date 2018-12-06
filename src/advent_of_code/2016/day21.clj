(ns advent-of-code.2016.day21
  "Twenty-first day's solutions for the Advent of Code 2016"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]))

(def puzzle
  "This is the source "
  (->> "resources/2016/input/day21.txt"
       (io/reader)
       (line-seq)))

(defn swap-pos
  "Function to swap the characters at positions the two positions - zero-biased."
  [pw {i :x j :y}]
  (let [t (nth pw i)]
    (-> (vec pw)
      (assoc i (nth pw j))
      (assoc j t)
      (as-> v (apply str v)))))

(defn swap-ltr
  "Function to swap all instances of `:a` with `:b` each place it appears."
  [pw {a :a b :b}]
  (let [a' (if (string? a) (first a) a)
        b' (if (string? b) (first b) b)
        swp (fn [c] (cond
                      (= a' c) b'
                      (= b' c) a'
                      :else c))]
    (apply str (map swp pw))))

(defn rotate-cnt
  "Function to rotate the entire string either :left or :right the given
  number of positions. Looping around the end in either direction, as needed."
  [pw {dir :dir cnt :cnt}]
  (if (= :right dir)
    (let [cz (- (count pw) cnt)]
      (str (subs pw cz) (subs pw 0 cz)))
    (str (subs pw cnt) (subs pw 0 cnt))))

(defn rotate-pos
  "Function to rotate to the right based on the index of `:a` - n+1 times,
  plus 1 if n => 4."
  [pw {a :a}]
  (let [a' (if (string? a) a (str a))
        i (.indexOf pw a')]
    (rotate-cnt pw {:dir :right :cnt (mod (+ 1 i (if (<= 4 i) 1 0)) (count pw))})))

(defn rev-pos
  "Function to reverse the characters in the substring from indexes `:x` to
  `:y`."
  [pw {i :x j :y}]
  (apply str
    (concat
      (take i pw) (reverse (take (inc (- j i)) (drop i pw))) (drop (inc j) pw))))

(defn move
  "Function to remove the character at position `:x` and then insert it
  back into the string so that it appears at position `:y`."
  [pw {i :x j :y}]
  (let [c (nth pw i)
        cut (concat (take i pw) (drop (inc i) pw))]
    (apply str
      (concat (take j cut) [c] (drop j cut)))))

(defn code-it
  "Function to do the simple parsing of the input of the rules for scrambling
  the password."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))]
    (cond
      (string? s) (cond
                    (.startsWith s "swap position")
                      (let [pts (drop 1 (re-matches #"^swap position (\d+) with position (\d+)$" s))]
                        {:action swap-pos
                         :args {:x (pint (first pts)) :y (pint (second pts))}})
                    (.startsWith s "swap letter")
                      (let [pts (drop 1 (re-matches #"^swap letter (\S) with letter (\S)$" s))]
                        {:action swap-ltr
                         :args {:a (first pts) :b (second pts)}})
                    (.startsWith s "rotate based")
                      (let [pts (drop 1 (re-matches #"^rotate based on position of letter (\S)$" s))]
                        {:action rotate-pos
                         :args {:a (first pts)}})
                    (.startsWith s "rotate")
                      (let [pts (drop 1 (re-matches #"^rotate (\S+) (\d+) step(s){0,1}$" s))]
                        {:action rotate-cnt
                         :args {:dir (keyword (first pts)) :cnt (pint (second pts))}})
                    (.startsWith s "reverse")
                      (let [pts (drop 1 (re-matches #"^reverse positions (\d+) through (\d+)$" s))]
                        {:action rev-pos
                         :args {:x (pint (first pts)) :y (pint (second pts))}})
                    (.startsWith s "move")
                      (let [pts (drop 1 (re-matches #"^move position (\d+) to position (\d+)$" s))]
                        {:action move
                         :args {:x (pint (first pts)) :y (pint (second pts))}}))
      (coll? s)   (map code-it s)
      :else       s)))

(defn one
  "Function to use the puzzle instructions and scramble the password."
  [& [src]]
  (let [pwi (or src puzzle)]
    (loop [pw "abcdefgh"
           ss (code-it pwi)]
      (let [s (first ss)]
        (if s
          (recur ((:action s) pw (:args s)) (rest ss))
          pw)))))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src ["swap position 4 with position 0"
             "swap letter d with letter b"
             "reverse positions 0 through 4"
             "rotate left 1 step"
             "move position 1 to position 4"
             "move position 3 to position 0"
             "rotate based on position of letter b"
             "rotate based on position of letter d"]]
    (loop [pw "abcde"
           ss (code-it src)]
      (let [s (first ss)]
        (if s
          (recur ((:action s) pw (:args s)) (rest ss))
          pw)))))
