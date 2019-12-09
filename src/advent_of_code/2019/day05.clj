(ns advent-of-code.2019.day05
  "Fifth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int not-zero? mk-seq un-seq
                                         parse-bool]]
            [advent-of-code.2019.day02 :refer [vat]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the Intcode program to run."
  (-> (slurp "resources/2019/input/day05.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-int))))

(def trial1
  "Test data for the first part of the puzzle"
  [1002 4 3 4 33])

(def trial2
  "Test data for the first part of the puzzle"
  [1101 100 -1 4 0])

(defn run
  "Function to take the memory map, and a program counter, and execute the
  Intcode program starting at that address and ending when the program
  successfully exits."
  [mems & [arg pc]]
  (let [no-io? (not (parse-bool (:io-wait mems)))
        rbos (atom 0)]
    (loop [mem (vec (if (map? mems) (:memory mems) mems))
           ip (or (if (map? mems) (:pc mems) pc) 0)
           ins (if-let [v (if (map? mems) (:input mems) arg)] (mk-seq v))
           outs (transient (if (map? mems) (vec (:output mems)) []))]
      (let [inst (nth mem ip nil)
            op (mod inst 100)
            mde (reverse (drop-last 2 (str inst)))
            adr (fn [ipos]
                  (case (nth mde (dec ipos) \0)
                    \0 (nth mem (+ ipos ip) nil)
                    \1 (+ ipos ip)
                    \2 (+ (nth mem (+ ipos ip) 0) @rbos)))
            ld (fn [ipos] (nth mem (adr ipos) 0))
            st (fn [ad v]
                 (let [msz (count mem)]
                   (if (< ad msz)
                     (assoc mem ad v)
                     (assoc (vec (concat mem (repeat (inc (- ad msz)) 0))) ad v))))]
        (case op
          (1 2) (let [a (ld 1)
                      b (ld 2)
                      tgt (adr 3)]
                  (recur (st tgt ((if (= op 1) + *) a b)) (+ 4 ip) ins outs))
          3     (let [inp (first ins)
                      tgt (adr 1)]
                  (if (or inp no-io?)
                    (recur (st tgt (or inp 0)) (+ 2 ip) (rest ins) outs)
                    {:output (persistent! outs) :memory mem :input nil :pc ip :state :io-in}))
          4     (let [ov (ld 1)]
                  (if no-io?
                    (recur mem (+ 2 ip) ins (conj! outs ov))
                    {:output (persistent! (conj! outs ov)) :memory mem :input ins :pc (+ 2 ip) :state :io-out}))
          (5 6) (let [tst (ld 1)
                      nip (ld 2)]
                  (recur mem (if ((if (= op 5) not-zero? zero?) tst) nip (+ 3 ip)) ins outs))
          (7 8) (let [a (ld 1)
                      b (ld 2)
                      tgt (adr 3)]
                  (recur (st tgt (if ((if (= op 7) < =) a b) 1 0)) (+ 4 ip) ins outs))
          9     (let [os (ld 1)]
                  (swap! rbos + os)
                  (recur mem (+ 2 ip) ins outs))
          99    {:output (persistent! outs) :memory mem :input ins :pc ip :state :halt}
          {:output (persistent! outs) :memory mem :input ins :pc ip :state :exception})))))

(defn yoyo
  "Run a bunch of tests on the trial input. The output should be a series of
  0, 1, ... 999, 1000, 1001 - which means it's all working as it's supposed
  to."
  []
  (doseq [[mem inp] [[[3 9 8 9 10 9 4 9 99 -1 8] 5] [[3 9 8 9 10 9 4 9 99 -1 8] 8]
                     [[3 9 7 9 10 9 4 9 99 -1 8] 9] [[3 9 7 9 10 9 4 9 99 -1 8] 7]
                     [[3 3 1108 -1 8 3 4 3 99] 5] [[3 3 1108 -1 8 3 4 3 99] 8]
                     [[3 3 1107 -1 8 3 4 3 99] 9] [[3 3 1107 -1 8 3 4 3 99] 7]
                     [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] 0]
                     [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] 1]
                     [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 0]
                     [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 1]
                     [[3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] 7]
                     [[3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] 8]
                     [[3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] 9]
                     ]]
    (prn (un-seq (:output (run mem inp))))
    ))

(defn bobo
  "Testing out the one simluation looking for an issue. Got it!"
  []
  (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 0))

(defn one
  "Function to run the program with an input of 1, and toss the memory at
  the end because the printing output will be the log of all we need."
  [& coll]
  (let [inp (vec (or coll puzzle))]
    (:output (run inp 1))))

(defn two
  "Function to run the program with an input of 5, and toss the memory at
  the end because the printing output will be the log of all we need."
  [& coll]
  (let [inp (vec (or coll puzzle))]
    (:output (run inp 5))))
