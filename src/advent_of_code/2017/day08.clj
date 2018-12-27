(ns advent-of-code.2017.day08
  "Eighth day's solutions for the Advent of Code 2017"
  (require [advent-of-code.util :refer [parse-int sum]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(defn code-it
  "Function to parse the data from the puzzle input into a usable format for
  processing."
  [s]
  (cond
    (string? s) (let [[dr cd amt tr ieq ta] (drop 1 (re-matches #"^(\S+) (\S+) ([-\d]+) if (\S+) ([<>=!]+) ([-\d]+)$" s))]
                  {:tgt-reg dr
                   :tgt-fn (if (= "inc" cd) + -)
                   :tgt-val (parse-int amt)
                   :tst-reg tr
                   :ineq (case ieq
                           ">" >
                           "<" <
                           ">=" >=
                           "<=" <=
                           "==" =
                           "!=" not=)
                   :tst-val (parse-int ta)})
    (coll? s)   (map code-it s)
    :else       s))

(def puzzle
  "This is the program input that we get from the puzzle."
  (->> "resources/2017/input/day08.txt"
    (io/reader)
    (line-seq)
    (code-it)))

(def sample
  "This is the example program in the puzzle."
  (->> ["b inc 5 if a > 1"
        "a inc 1 if b < 5"
        "c dec -10 if a >= 1"
        "c inc -20 if c == 10"]
    (code-it)))

(defn run
  "Function to start with a set of register values, and a program, and run
  through the program updating the registers based on the instructions. The
  result will be the output of the registers based on the changes from the
  code."
  [reg pgm]
  (let [chk (fn [m k] (if (nil? (m k)) (assoc m k 0) m))
        hrv (atom 0)]
    (loop [instr pgm
           regs reg]
      (if-let [inst (first instr)]
        (let [cln (chk (chk regs (:tgt-reg inst)) (:tst-reg inst))
              ok? ((:ineq inst) (get cln (:tst-reg inst)) (:tst-val inst))
              nxt (if ok?
                    (update cln (:tgt-reg inst) (:tgt-fn inst) (:tgt-val inst))
                    cln)
              mrv (apply max (vals nxt))]
          (if (< @hrv mrv) (reset! hrv mrv))
          (recur (rest instr) nxt))
        (assoc regs :highest @hrv)))))

(defn one
  "Function to find out the largest value in the registers after the program
  has run."
  []
  (->> (run {} puzzle)
    (seq)
    (sort-by second >)))

(defn two
  "Function to run the code and just show the highest recorded register value
  while the program was running."
  []
  (:highest (run {} puzzle)))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (run {} sample))
