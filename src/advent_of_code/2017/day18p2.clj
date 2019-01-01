(ns advent-of-code.2017.day18p2
  "Eighteenth day's solution part 2 for the Advent of Code 2017"
  (:require [advent-of-code.2017.day18p1 :refer [puzzle rget seti add mul modi
                                                 jgz init-reg]
                                         :as p1]
            [advent-of-code.util :refer [parse-int is-int-char?]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def sample
  "This is the skip-factor for the sample spinlock."
  ["snd 1"
   "snd 2"
   "snd p"
   "rcv a"
   "rcv b"
   "rcv c"
   "rcv d"])

(def queues
  "These are the queues for each of the processes that run. We need them to
  be defined outside the `rcv` and `snd` - so that they are shared."
  {0 (atom []) 1 (atom [])})

(def counts
  "These are the send counts for each of the processes that run. We need
  them to be defined outside the `rcv` and `snd` - so that they are shared."
  {0 (atom 0) 1 (atom 0)})

(defn snd
  "Function that sends the value of X to the other program. These values
  wait in a queue until that program is ready to receive them. Each program
  has its own message queue, so a program can never receive a message it sent."
  [regs x]
  (let [pid (:pid @regs)
        dst (mod (inc pid) 2)
        v (rget regs x)]
    (swap! (get counts pid) inc)
    (swap! (get queues dst) conj v)
    (infof "[pid:%s] sending value of %s to pid: %s (tot: %s/%s)" pid v dst @(get counts 0) @(get counts 1))
    1))

(defn rcv
  "Function that receives the next value and stores it in register X. If
  no values are in the queue, the program waits for a value to be sent to
  it. Programs do not continue to the next instruction until they have
  received a value. Values are received in the order they are sent."
  [regs x]
  (let [pid (:pid @regs)
        q (get queues pid)]
    (if (not-empty @q)
      (let [v (first @q)]
        (infof "[pid:%s] popped value %s to %s (%s in queue)" pid v x (count (rest @q)))
        (swap! regs assoc x v)
        (swap! q (comp vec rest))
        1)
      0)))

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
                    (.startsWith s "rcv")
                      (let [args (drop 1 (re-matches #"^rcv (.+)$" s))]
                        {:inst rcv :args (mapv key-or-val args)})
                    :else (p1/code-it s))
      (coll? s)   (mapv code-it s)
      :else       s)))

(defn run
  "Function to run two programs side-by-side, each getting one instruction
  each, and then seeing if we can continue, or if we need to stop because
  the two processes are deadlocked... or out of instructions."
  [pgms & [rs]]
  (let [cqc (fn [] [(count @(get queues 0)) (count @(get queues 1))])
        csc (fn [] [@(get counts 0) @(get counts 1)])
        base (or rs (init-reg pgms))
        reg0 (atom (assoc base :pid 0 :p 0))
        reg1 (atom (assoc base :pid 1 :p 1))]
    ;; clear out the queues and counts before we get started
    (doseq [p [0 1]]
      (reset! (get counts p) 0)
      (reset! (get queues p) []))
    ;; loop over a step in one, and then the other...
    (loop [pc0 0
           pc1 0]
      (let [i0 (nth pgms pc0 nil)
            i1 (nth pgms pc1 nil)]
        (if (and i0 i1)
          (let [pc0' (+ pc0 (apply (:inst i0) (concat [reg0] (:args i0))))
                pc1' (+ pc1 (apply (:inst i1) (concat [reg1] (:args i1))))]
            (if (and (= pc0 pc0') (= pc1 pc1') (= [0 0] (cqc)))
              {:reg0 @reg0 :reg1 @reg1 :status "deadlock" :sent (csc) :pc [pc0' pc1'] :queues (cqc)}
              (recur pc0' pc1')))
          {:reg0 @reg0 :reg1 @reg1 :status "EOP" :sent (csc) :pc [pc0 pc1] :queues (cqc)})))))

(defn two
  "Function to run the complete puzzle until we run into a problem."
  []
  (run (code-it puzzle)))

(defn bobo
  "Function to run the sample program until we hit something to stop."
  []
  (run (code-it sample)))
