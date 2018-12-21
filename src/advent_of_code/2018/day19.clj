(ns advent-of-code.2018.day19
  "Nineteenth day's solutions for the Advent of Code 2018"
  (require [advent-of-code.2016.day25 :refer [->int]]
           [advent-of-code.2018.day16 :refer [addr addi mulr muli banr bani borr
                                              bori setr seti gtir gtri gtrr eqir
                                              eqri eqrr]]
           [clojure.java.io :as io]
           [clojure.set :refer [map-invert]]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def operations
  "This is a simple mapping from the string name of the operation to it's
  function so that it makes running the code all that easier."
  {"addr" addr
   "addi" addi
   "mulr" mulr
   "muli" muli
   "banr" banr
   "bani" bani
   "borr" borr
   "bori" bori
   "setr" setr
   "seti" seti
   "gtir" gtir
   "gtri" gtri
   "gtrr" gtrr
   "eqir" eqir
   "eqri" eqri
   "eqrr" eqrr})

(def op-names
  "This is the reverse of the 'operations' map, and will be used for the
  nice logging of the program as it runs."
  (map-invert operations))

(def puzzle
  "This is the input of the forest."
  (-> (slurp "resources/2018/input/day19.txt")
      (cs/split #"\n")))

(def sample
  "This is the sample forest in the puzzle statement."
  ["#ip 0"
   "seti 5 0 1"
   "seti 6 0 2"
   "addi 0 1 0"
   "addr 1 2 3"
   "setr 1 0 0"
   "seti 8 0 4"
   "seti 9 0 5"])

(defn code-it
  "Function to do a simple conversion of the string instructions to functions
  and integers so that the code will execute a lot faster as we only have to
  'compile' this code segment once."
  [s]
  (cond
    (string? s)
      (let [[ins as bs cs] (drop 1 (re-matches #"^(\S+) (\d+) (\d+) (\d+)$" s))]
        [(get operations ins) (->int as) (->int bs) (->int cs)])
    (coll? s)
      (mapv code-it s)
    :else
      s))

(defn run
  "Function to run a program, with a stargin set of registers, and an IP addr,
  and starting program counter, and run it all the way to it's completion -
  returning the registers when done."
  [reg pgm ip spc]
  (let [tot (loop [pc spc
                   cnt 0]
              (if-let [row (nth pgm pc nil)]
                (let [[ins a b c] row]
                  (swap! reg assoc ip pc)
                  (ins reg a b c)
                  (infof "ip=%s : %s %s %s %s : %s" pc (get op-names ins) a b c @reg)
                  (recur (inc (nth @reg ip)) (inc cnt)))
                cnt))]
    {:reg @reg :steps tot}))

(defn one
  "Function to run the background process with the addition of the IP addr
  so that we can do jumps. The key is the register state when it's all done."
  []
  (let [raw puzzle
        ip (->> (re-matches #"^#ip (\d+)$" (first raw))
             (second)
             (->int))
        pgm (code-it (drop 1 raw))
        reg (atom [0 0 0 0 0 0])]
    (run reg pgm ip 0)))

;; part 2 is looking at the code and seeing that it's summing the factors
;; of the value computed in reg 4. And for Part 2, that's 10551326. This
;; sum is 15826992.

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [raw sample
        ip (->> (re-matches #"^#ip (\d+)$" (first raw))
             (second)
             (->int))
        pgm (code-it (drop 1 raw))
        reg (atom [0 0 0 0 0 0])]
    (run reg pgm ip 0)))
