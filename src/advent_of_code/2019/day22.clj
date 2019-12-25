(ns advent-of-code.2019.day22
  "Twenty-second day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.core.memoize :as memo]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the shuffle operations to perform on a new deck."
  (-> (slurp "resources/2019/input/day22.txt")
      (cs/trim)
      (cs/split #"\n")))

(def trial1
  "Test data for part 1 - final order of small deck: (0 3 6 9 2 5 8 1 4 7)"
  ["deal with increment 7"
   "deal into new stack"
   "deal into new stack"])

(def trial2
  "Test data for part 1 - final order of small deck: (3 0 7 4 1 8 5 2 9 6)"
  ["cut 6"
   "deal with increment 7"
   "deal into new stack"])

(def trial3
  "Test data for part 1 - final order of small deck: (6 3 0 7 4 1 8 5 2 9)"
  ["deal with increment 7"
   "deal with increment 9"
   "cut -2"])

(def trial4
  "Test data for part 1 - final order of small deck: (9 2 5 8 1 4 7 0 3 6)"
  ["deal into new stack"
   "cut -2"
   "deal with increment 7"
   "cut 8"
   "cut -4"
   "deal with increment 7"
   "cut 3"
   "deal with increment 9"
   "deal with increment 3"
   "cut -1"])

(defn new-stack
  "Function to implement the 'new-stack' operation, which is pretty simple."
  [s]
  (reverse s))

(defn cut-n-cards
  "Function to implement the 'cut cards' operation - both positive and negative."
  [s n]
  (if (pos? n)
    (concat (drop n s) (take n s))
    (concat (take-last (Math/abs n) s) (drop-last (Math/abs n) s))))

(defn deal
  "Function to implement the 'deal n' operation which is a little interesting,
  in that we use an atom to make it easy to place the cards in the right order,
  but that's still not too bad."
  [s n]
  (let [cnt (count s)
        ans (transient (vec (repeat cnt 0)))]
    (doseq [[c i] (map vector s (range))
            :let [idx (mod (* n i) cnt)]]
      (assoc! ans idx c))
    (persistent! ans)))

(defn dealer
  "Function to take a series of shuffle operations, shs, and a deck of cards,
  and run all the operations, in order, on the deck - returning the output of
  the deck to the caller."
  [shs deck]
  (loop [src shs
         dk deck]
    (if-let [l (first src)]
      (cond
        (= "deal into new stack" l)
          (recur (rest src) (new-stack dk))
        (.startsWith l "cut ")
          (let [a (parse-int (last (re-matches #"cut (.*)" l)))]
            (recur (rest src) (cut-n-cards dk a)))
        (.startsWith l "deal with increment ")
          (let [a (parse-int (last (re-matches #"deal with increment (.*)" l)))]
            (recur (rest src) (deal dk a))))
      dk)))

(defn one
  "Function to run the puzzle input on the factory new deck of 10007 cards, and
  find the location of the card with '2019' on it."
  []
  (.indexOf (dealer puzzle (range 10007)) 2019))

(defn xgcd
  "Extended Euclidean Algorithm. Returns [gcd(a,b) x y] where
  ax + by = gcd(a,b)."
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (- y (* (Math/floorDiv (long b) (long a)) x)) x])))

(defn modinv*
  "Function that returns the inverse of the modulo value based on the values
  being coprimes, and that the Extended Euclidean Algorithm is used in the
  generation. This will make it possible to *reverse* the 'deal with increment'
  command directly without having to iterate to find the answer."
  [a b]
  (let [[g x _] (xgcd a b)]
    (if (= 1 g) (mod x b))))

(def modinv
  "Memoized function that returns the inverse of the modulo value based on
  the values being coprimes, and that the Extended Euclidean Algorithm is
  used in the generation. This will make it possible to *reverse* the 'deal
  with increment' command directly without having to iterate to find the
  answer."
  (memo/lru modinv* :lru/threshold 200))

(defn modpow
  "b^e mod m (using Java which solves some cases the pure clojure method
  has to be modified to tackle--i.e. with large b & e and calculation
  simplications when gcd(b, m) == 1 and gcd(e, m) == 1)"
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn rdealer
  "Function to reverse the deal stack provided, getting the *starting* position
  of the card as opposed to the ending position, and to do it efficiently with
  the coprime function to get the movinv. This is not possible without seeing
  this, and it was something I certainly had to look up."
  [shs sz fpos]
  (loop [src (reverse shs)
         pos fpos]
    (if-let [l (first src)]
      (cond
        (= "deal into new stack" l)
          (recur (rest src) (- sz pos 1))
        (.startsWith l "cut ")
          (let [a (parse-int (last (re-matches #"cut (.*)" l)))
                aa (Math/abs a)
                os (- sz aa)]
            (if (pos? a)
              (if (<= os pos)
                (recur (rest src) (- pos os))
                (recur (rest src) (+ pos aa)))
              (if (< pos aa)
                (recur (rest src) (+ pos os))
                (recur (rest src) (- pos aa)))))
        (.startsWith l "deal with increment ")
          (let [a (parse-int (last (re-matches #"deal with increment (.*)" l)))]
            (if (zero? pos)
              (recur (rest src) 0)
              (recur (rest src) (mod (* pos (bigint (modinv a sz))) sz)))))
      pos)))

(defn two
  "Function to calculate the position of the card shuffle when it's done
  a very large number of times. This was pulled from a hint on the site
  where a few things I would not have guessed: 1) the process is linear -
  Face it, it's got a modulo in it - how can this be linear - but I guess
  the point is that it's linear 'mod sz'... so OK... I guess... and the
  second was that if you have an 'a*x +b' and you want to perform it
  'n' times, it's possible to expand it like a polynomial, and then use
  a 'modpow' function from BigInteger to do the exponentiation. I get
  it... kinda... but I'll have to remember this craziness..."
  []
  (let [n 101741582076661
        sz 119315717514047
        f (partial rdealer puzzle sz)
        x 2020
        y (f x)
        z (f y)
        a (mod (* (- y z) (modinv (+ (- x y) sz) sz)) sz)
        b (mod (- y (* a x)) sz)
        q (modpow a n sz)]
    (long (mod (+ (* q x) (* (- q 1) (modinv (- a 1) sz) b)) sz))))
