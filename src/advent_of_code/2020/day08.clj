(ns advent-of-code.2020.day08
  "Eighth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a line of input and parse it into a useful instruction set
  map that makes it easy to operate them in the CPU that we'll be making."
  [s]
  (let [pts (split (trim s) " ")
        arg (cs/replace (second pts) "+" "")]
    {:op (keyword (first pts)) :arg (parse-int arg)}))

(def puzzle
  "This is the input of the instructions in the game console."
  (-> (slurp "resources/2020/input/day08.txt")
      (trim)
      (split #"\n")
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["nop +0"
       "acc +1"
       "jmp +4"
       "acc +3"
       "jmp -3"
       "acc -99"
       "acc +1"
       "jmp -4"
       "acc +6"]
      (->> (map parse))))

(defn exec
  "Function to execute the provided boot program starting the PC at 0, and
  tracking the accumulator as well as the PC as we execute the different
  instructions. We stop on the first instruction that is going to be executed
  for a second time."
  [pgm]
  (let [acc (atom 0)
        lastpc (dec (count pgm))]
    (loop [pc 0
           hits (transient #{})]
      (cond
        (hits pc)
          {:acc @acc :pc pc :why "infinite loop"}
        (not (<= 0 pc lastpc))
          {:acc @acc :pc pc :why "terminate"}
        :else
          (let [{op :op arg :arg} (nth pgm pc)]
            ; (infof "pc: %d ... acc: %d ... on: %s %d" pc @acc (name op) arg)
            (case op
              :acc (do (swap! acc + arg) (recur (inc pc) (conj! hits pc)))
              :jmp (recur (+ pc arg) (conj! hits pc))
              :nop (recur (inc pc) (conj! hits pc))))))))

(defn one
  "Function to run the provided program and find the value of the accumulator
  right before the program is about to execute any instruction twice. This
  will mean it's in an infinite loop, and that's what we want to avoid."
  [& [coll]]
  (exec (or coll puzzle)))

(defn two
  "Function to search all the 'nop' and 'jmp' instructions to see which is
  the wrong one, and run each modified program to see if it terminates. If
  so, then we're done, and report on the findings. If not, try the next
  instruction flip and continue."
  [& [coll]]
  (let [src (vec (or coll puzzle))
        flip (fn [[i {op :op}]] [i (if (= :nop op) :jmp :nop)])
        tsts (->> (map vector (range) src)
                  (filter #(#{:nop :jmp} (:op (second %))))
                  (map flip))]
    (loop [chgs tsts]
      (if-let [[i op] (first chgs)]
        (let [upd (exec (assoc-in src [i :op] op))]
          (if (= (:why upd) "terminate")
            {:at i :to op :got upd}
            (recur (rest chgs))))))))
