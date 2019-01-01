(ns advent-of-code.2018.day16
  "Sixteenth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn pull-sample
  "Function to pull all the samples from my *edited* input from the site. I
  put all the before/after sets into one line so that they would be easier
  to parse, and that worked out perfectly."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))
        pts (->> (re-matches #"^Before:\s+\[(\d+), (\d+), (\d+), (\d+)\] (\d+) (\d+) (\d+) (\d+) After:\s+\[(\d+), (\d+), (\d+), (\d+)\]$" s)
              (drop 1)
              (map pint))]
    (if (= 12 (count pts))
      {:before (vec (take 4 pts))
       :op (nth pts 4)
       :a (nth pts 5)
       :b (nth pts 6)
       :c (nth pts 7)
       :after (vec (take-last 4 pts))})))

(def samples
  "This is the input of the samples of the instructions I've decoded from the
  wrist unit."
  (->> "resources/2018/input/day16.txt"
    (io/reader)
    (line-seq)
    (map pull-sample)
    (remove nil?)))

(defn pull-instruction
  "Function to pull all the instructions from my *edited* input from the site.
  I put all the before/after sets into one line so that they would be easier
  to parse, and that worked out perfectly."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))
        pts (->> (re-matches #"^(\d+) (\d+) (\d+) (\d+)$" s)
              (drop 1)
              (map pint))]
    (if (= 4 (count pts))
      (vec pts))))

(def instructions
  "This is the input of the computing instructions for a program I've decoded
  from the wrist unit."
  (->> "resources/2018/input/day16.txt"
    (io/reader)
    (line-seq)
    (map pull-instruction)
    (remove nil?)))

(defn addr
  "(add register) stores into register C the result of adding register A and
  register B."
  [reg a b c]
  (swap! reg assoc c (+ (nth @reg a) (nth @reg b))))

(defn addi
  "(add immediate) stores into register C the result of adding register A
  and value B."
  [reg a b c]
  (swap! reg assoc c (+ (nth @reg a) b)))

(defn mulr
  "(multiply register) stores into register C the result of multiplying
  register A and register B."
  [reg a b c]
  (swap! reg assoc c (* (nth @reg a) (nth @reg b))))

(defn muli
  "(multiply immediate) stores into register C the result of multiplying
  register A and value B."
  [reg a b c]
  (swap! reg assoc c (* (nth @reg a) b)))

(defn banr
  "(bitwise AND register) stores into register C the result of the bitwise
  AND of register A and register B."
  [reg a b c]
  (swap! reg assoc c (bit-and (nth @reg a) (nth @reg b))))

(defn bani
  "(bitwise AND immediate) stores into register C the result of the bitwise
  AND of register A and value B."
  [reg a b c]
  (swap! reg assoc c (bit-and (nth @reg a) b)))

(defn borr
  "(bitwise OR register) stores into register C the result of the bitwise OR
  of register A and register B."
  [reg a b c]
  (swap! reg assoc c (bit-or (nth @reg a) (nth @reg b))))

(defn bori
  "(bitwise OR immediate) stores into register C the result of the bitwise OR
  of register A and value B."
  [reg a b c]
  (swap! reg assoc c (bit-or (nth @reg a) b)))

(defn setr
  "(set register) copies the contents of register A into register C.
  (Input B is ignored.)"
  [reg a b c]
  (swap! reg assoc c (nth @reg a)))

(defn seti
  "(set immediate) stores value A into register C. (Input B is ignored.)"
  [reg a b c]
  (swap! reg assoc c a))

(defn gtir
  "(greater-than immediate/register) sets register C to 1 if value A is
  greater than register B. Otherwise, register C is set to 0."
  [reg a b c]
  (swap! reg assoc c (if (< (nth @reg b) a) 1 0)))

(defn gtri
  "(greater-than register/immediate) sets register C to 1 if register A
  is greater than value B. Otherwise, register C is set to 0."
  [reg a b c]
  (swap! reg assoc c (if (< b (nth @reg a)) 1 0)))

(defn gtrr
  "(greater-than register/register) sets register C to 1 if register A
  is greater than register B. Otherwise, register C is set to 0."
  [reg a b c]
  (swap! reg assoc c (if (< (nth @reg b) (nth @reg a)) 1 0)))

(defn eqir
  "(equal immediate/register) sets register C to 1 if value A is equal to
  register B. Otherwise, register C is set to 0."
  [reg a b c]
  (swap! reg assoc c (if (= (nth @reg b) a) 1 0)))

(defn eqri
  "(equal register/immediate) sets register C to 1 if register A is equal
  to value B. Otherwise, register C is set to 0."
  [reg a b c]
  (swap! reg assoc c (if (= b (nth @reg a)) 1 0)))

(defn eqrr
  "(equal register/register) sets register C to 1 if register A is equal
  to register B. Otherwise, register C is set to 0."
  [reg a b c]
  (swap! reg assoc c (if (= (nth @reg b) (nth @reg a)) 1 0)))

(defn matches
  "Function to look at one of the example sets of registers before, and after,
  along with the complete instruction, and see how many of the operands fit
  that data. The number of matches is returned as an integer."
  [bef _ a b c aft & [operands]]
  (let [oprs (or operands [addr addi mulr muli banr bani borr bori setr seti
                           gtir gtri gtrr eqir eqri eqrr])
        hits (atom [])]
    (doseq [op oprs
            :let [reg (atom bef)]]
      (op reg a b c)
      (if (= aft @reg) (swap! hits conj op)))
    @hits))

(def operands
  "These are determined by elimination of the matches when only one opcode
  can possibly match the behavior. Repeat to get all 16."
  { 0 addi
    1 bani
    2 gtir
    3 borr
    4 eqrr
    5 bori
    6 gtrr
    7 setr
    8 muli
    9 seti
   10 banr
   11 gtri
   12 eqir
   13 eqri
   14 addr
   15 mulr })

(defn one
  "Function to return the number of samples whose behavior matches 3 or more
  opcodes. This uses `matches` to compute the number, and then we just
  filter on that, and count them up."
  []
  (let [lim 3
        cnt (atom 0)]
    (doseq [{op :op a :a b :b c :c bef :before aft :after :as ex} samples
            :let [hits (count (matches bef op a b c aft))]
            :when (<= lim hits)]
      (swap! cnt inc))
    @cnt))

(defn two
  "Function to run the code in the puzzle with the decoded operands and see
  what the values are for the registers at the end."
  []
  (let [reg (atom [0 0 0 0])]
    (doseq [[op a b c] instructions]
      ((get operands op) reg a b c))
    @reg))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [all [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr]
        tsts (remove (set (vals operands)) all)]
    (->> (for [{op :op a :a b :b c :c bef :before aft :after :as ex} samples
               :let [hits (matches bef op a b c aft tsts)]
               :when (not-empty hits)]
           {:op op :cnt (count hits) :hits hits})
         (sort-by :cnt))))
