(ns advent-of-code.2018.day07
  "Seventh day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs])
  (import [java.util ArrayList]))

(def puzzle
  "This is the source list of all elf fabric claims."
  (->> "resources/2018/input/day07.txt"
    (io/reader)
    (line-seq)))

(defn code-it
  "Function to parse the puzzle input into something that's useful for solving."
  [s]
  (cond
    (string? s) (drop 1 (re-matches #"^Step (\S) must be finished before step (\S) can begin\.$" s))
    (coll? s)   (map code-it s)
    :else       s))

(defn one
  "Function to put all the instructions in the right order from the puzzle so
  that the elves can assemble the sleigh properly for Santa."
  [& [src]]
  (let [si (code-it (or src puzzle))
        ops (ArrayList.)
        dep (into {} (for [[k v] (group-by second si)] [k (map first v)]))
        need? (fn [c] (neg? (.indexOf ops c)))
        got? (complement need?)]
    ; add those with no dependenciies at all
    (.addAll ops (sort (remove (set (map second si)) (set (flatten si)))))
    ; now we need to add the first sorted available step over and over
    (loop []
      (let [ads (for [[k v] dep :when (and (need? k) (every? got? v))] k)]
        (when (not-empty ads)
          (.add ops (first (sort ads)))
          (recur))))
    ; return the string from the steps
    (apply str ops)))

(defn two
  "Function to figure out how long it would take with 5 workers and a 60-sec
  offset on the name of the step, and all the rules from the puzzle."
  []
  (let [si (code-it puzzle)
        ops (ArrayList.)
        dep (-> (into {} (for [[k v] (group-by second si)] [k (map first v)]))
              (merge (into {} (for [k (remove (set (map second si)) (set (flatten si)))] [k []]))))
        need? (fn [c] (neg? (.indexOf ops c)))
        got? (complement need?)
        dur (fn [c] (+ 61 (- (int (first c)) (int \A))))
        ts (atom 0)
        work (atom {})
        max-wk 5 ;;2
        run? (fn [c] (get @work c))
        ready (fn [] (for [[k v] dep :when (and (need? k) (every? got? v))] k))]
    (loop []
      (let [ads (ready)]
        (when (not-empty ads)
          ; do one unit of work on all workers
          (doseq [c (keys @work)] (swap! work update c dec))
          ; put the finishers on the output
          (doseq [[c t] @work
                  :when (= 0 t)]
            (.add ops c)
            (swap! work dissoc c))
          ; add to the workers, as available
          (doseq [c (take (- max-wk (count @work)) (filter #(not (run? %)) (ready)))]
            (swap! work assoc c (dur c)))
          (swap! ts inc)
          (recur))))
    {:time (dec @ts) :steps (apply str ops)}))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [
        ; si (code-it ["Step C must be finished before step A can begin."
        ;              "Step C must be finished before step F can begin."
        ;              "Step A must be finished before step B can begin."
        ;              "Step A must be finished before step D can begin."
        ;              "Step B must be finished before step E can begin."
        ;              "Step D must be finished before step E can begin."
        ;              "Step F must be finished before step E can begin."])
        si (code-it puzzle)
        ops (ArrayList.)
        dep (-> (into {} (for [[k v] (group-by second si)] [k (map first v)]))
              (merge (into {} (for [k (remove (set (map second si)) (set (flatten si)))] [k []]))))
        need? (fn [c] (neg? (.indexOf ops c)))
        got? (complement need?)
        dur (fn [c] (+ 61 (- (int (first c)) (int \A))))
        ts (atom 0)
        work (atom {})
        max-wk 5 ;;2
        run? (fn [c] (get @work c))
        ready (fn [] (for [[k v] dep :when (and (need? k) (every? got? v))] k))
        ]
    (loop []
      (let [ads (ready)]
        (when (not-empty ads)
          ; do one unit of work on all workers
          (doseq [c (keys @work)] (swap! work update c dec))
          ; put the finishers on the output
          (doseq [[c t] @work
                  :when (= 0 t)]
            (.add ops c)
            (swap! work dissoc c))
          ; add to the workers, as available
          (doseq [c (take (- max-wk (count @work)) (filter #(not (run? %)) (ready)))]
            (swap! work assoc c (dur c)))

          (prn @ts @work (apply str ops))

          (swap! ts inc)
          (recur)))
    )
    {:time (dec @ts) :steps (apply str ops)}))
