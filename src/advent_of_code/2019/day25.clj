(ns advent-of-code.2019.day25
  "Twenty-fifth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum is-ascii?]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program - ASCII"
  (-> (slurp "resources/2019/input/day25.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn render
  "Function to take the output data from the Intcode ASCII computer output and
  turn it into a series of strings - and then pack all the lines into a vector
  as a complete log."
  [data]
  (loop [src data
         cur (transient [])
         lines (transient [])]
    (if-let [c (first src)]
      (cond
        (= 10 c)
          (recur (rest src) (transient []) (conj! lines (apply str (persistent! cur))))
        (is-ascii? c)
          (recur (rest src) (conj! cur (char c)) lines)
        :else
          (do
            (infof "invalid character value in Intcode output: %d" c)
            (recur (rest src) cur lines)))
      (persistent! (conj! lines (apply str (persistent! cur)))))))

(defn explore
  "Function to feed the Intcode computer a series of steps in this Adventure
  game, and then see what the outcome is."
  [cmds & [mem]]
  (let [ini (if (map? mem) mem {:memory (or mem puzzle)})
        inp (map int (str (cs/join "\n" cmds) "\n"))
        cmd (map int "Command?")]
    (loop [cpu (run (assoc ini :input inp :io-wait true))]
      (infof "cpu: %s" (pr-str (dissoc cpu :memory :output)))
      (infof "render:\n%s" (cs/join "\n" (render (:output cpu))))
      (if-not (or (= :halt (:state cpu)) (and (empty? (:input cpu)) (= cmd (take-last 8 (:output cpu)))))
        (recur (run (assoc cpu :io-wait true)))
        (view (:output cpu))))))

(defn one
  "Function to play the simple adventure game that is the droid investigating
  Santa's ship. It's all being run each time, so it's easy to manipulate what
  we need to get past the pressure plate."
  []
  (explore ["east" "take sand" "west"
            "south" "take ornament" "north"
            "west"
            "north" "take wreath"
            "east" "take fixed point" "west"
            "north" ;; "take infinite loop"
            "north" "take spool of cat6" "south" "south" "south"
            "south"  ;; at Arcade
            ; "take giant electromagnet"
            "south" "take candy cane" "north"
            "east"   ;; at Warp Drive Maintenance
            ; "take escape pod"
            "east"
            ; "south" "take photons" "north"
            "east" "take space law space brochure"
            "south" "take fuel cell"
            "inv"
            "south"
            "drop spool of cat6"   ;; too heavy all on it's own
            "drop ornament"        ;; too heavy all on it's own
            ; "drop wreath"
            ; "drop sand"
            ; "drop fixed point"
            "drop candy cane"
            ; "drop space law space brochure"
            "drop fuel cell"
            "west"]))
