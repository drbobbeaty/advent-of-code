(ns advent-of-code.2017.day18p1
  "Eighteenth day's solution for part 1 for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int is-int-char?]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the skip-factor for my spinlock."
  (-> (slurp "resources/2017/input/day18.txt")
    (cs/trim)
    (cs/split #"\n")))

(def sample
  "This is the skip-factor for the sample spinlock."
  ["set a 1"
   "add a 2"
   "mul a a"
   "mod a 5"
   "snd a"
   "set a 0"
   "rcv a"
   "jgz a -1"
   "set a 1"
   "jgz a -2"])

(defn rget
  "Function to get the value of the register, if the argument is a register,
  or return the number, if the value is a number. This will make the work in
  each instruction simpler."
  [regs v]
  (cond
    (number? v)  v
    (keyword? v) (get @regs v)))

(defn snd
  "Function that plays a sound with a frequency equal to the value of X"
  [regs x]
  (infof "playing a sound with frequency of %s" (rget regs x))
  (swap! regs assoc :freq (rget regs x))
  1)

(defn seti
  "Function that sets register X to the value of Y."
  [regs x y]
  (swap! regs assoc x (rget regs y))
  1)

(defn add
  "Function that increases register X by the value of Y."
  [regs x y]
  (swap! regs update x + (rget regs y))
  1)

(defn mul
  "Function that sets register X to the result of multiplying the value
  contained in register X by the value of Y"
  [regs x y]
  (swap! regs update x * (rget regs y))
  1)

(defn modi
  "Function that sets register X to the remainder of dividing the value
  contained in register X by the value of Y (that is, it sets X to the
  result of X modulo Y)."
  [regs x y]
  (swap! regs update x mod (rget regs y))
  1)

(defn rcv
  "Function that recovers the frequency of the last sound played, but
  only when the value of X is not zero. (If it is zero, the command does
  nothing.)"
  [regs x]
  (if-not (zero? (rget regs x))
    (infof "recovering: %s" (:freq @regs)))
  1)

(defn jgz
  "Function that jumps with an offset of the value of Y, but only if the
  value of X is greater than zero. (An offset of 2 skips the next
  instruction, an offset of -1 jumps to the previous instruction, and
  so on.)"
  [regs x y]
  (if (pos? (rget regs x))
    (rget regs y)
    1))

(defn code-it
  "Function to take the puzzle input and parse it into something we can
  really use easily in the execution of this sound board."
  [s]
  (let [key-or-val (fn [x] (if (every? is-int-char? x) (parse-int x) (keyword x)))]
    (cond
      (string? s) (cond
                    (.startsWith s "snd")
                      (let [args (drop 1 (re-matches #"^snd (.+)$" s))]
                        {:inst snd :args (mapv key-or-val args)})
                    (.startsWith s "set")
                      (let [args (drop 1 (re-matches #"^set (.+) (.+)$" s))]
                        {:inst seti :args (mapv key-or-val args)})
                    (.startsWith s "add")
                      (let [args (drop 1 (re-matches #"^add (.+) (.+)$" s))]
                        {:inst add :args (mapv key-or-val args)})
                    (.startsWith s "mul")
                      (let [args (drop 1 (re-matches #"^mul (.+) (.+)$" s))]
                        {:inst mul :args (mapv key-or-val args)})
                    (.startsWith s "mod")
                      (let [args (drop 1 (re-matches #"^mod (.+) (.+)$" s))]
                        {:inst modi :args (mapv key-or-val args)})
                    (.startsWith s "rcv")
                      (let [args (drop 1 (re-matches #"^rcv (.+)$" s))]
                        {:inst rcv :args (mapv key-or-val args)})
                    (.startsWith s "jgz")
                      (let [args (drop 1 (re-matches #"^jgz (.+) (.+)$" s))]
                        {:inst jgz :args (mapv key-or-val args)}))
      (coll? s)   (mapv code-it s)
      :else       s)))

(defn init-reg
  "Function to take a program, find all the registers in use in the program,
  and create an initial map of registers populated with zeros, so that we can
  start off with this and run as needed."
  [pgm]
  (->> (map :args pgm)
    (apply concat)
    (filter keyword?)
    (set)
    (map (fn [k] [k 0]))
    (into {})))

(defn run
  "Function to run a program with an optional set of registers, and log a
  few important milestones in the process. When the program is finished,
  we'll return the register map for analysis."
  [pgms & [rs]]
  (let [reg (atom (or rs (init-reg pgms)))]
    (loop [pc 0]
      (if-let [i (nth pgms pc nil)]
        (recur (+ pc (apply (:inst i) (concat [reg] (:args i)))))
        @reg))))

(defn one
  "Function to run the program and see what the first recovered frequency
  is. This is going to be an infinite loop, so you're going to have to
  kill it after it logs."
  []
  (run (code-it puzzle)))

(defn yoyo
  "Function to run the program and see what the first recovered frequency
  is. This is going to be an infinite loop, so you're going to have to
  kill it after it logs."
  []
  (run (code-it sample)))
