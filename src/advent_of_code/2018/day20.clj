(ns advent-of-code.2018.day20
  "Twentieth day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.2016.day25 :refer [->int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the forest."
  (-> (slurp "resources/2018/input/day19.txt")
      (cs/split #"\n")))

(defn chop
  "Function to chop up the string into parts that are direct, and those that
  are optional. This optionality could mean that there are multiple levels of
  optionality, and those have to be delt with, but this is chopping it up at
  a high level."
  [s]
  (let [ts (if (and (.startsWith s "^") (.endsWith s "$")) (.subSequence s 1 (dec (count s))) s)]
    (loop [src ts
           prts []]
      (let [idx (.indexOf src "(")]
        (if-not (neg? idx)
          (let [hdr (if (pos? idx) (subs src 0 idx))
                edx (loop [i (inc idx)
                           lvl 0]
                      (case (nth src i nil)
                        (\N \E \S \W \|) (recur (inc i) lvl)
                        \( (recur (inc i) (inc lvl))
                        \) (if (zero? lvl) i (recur (inc i) (dec lvl)))
                        nil i))
                ftr (not-empty (subs src idx (inc edx)))]
            (recur (subs src (inc edx)) (conj prts hdr ftr)))
          (if (not-empty src) (conj prts src) prts))))))

(defn fork
  "Function to fork apart the optional parts of a regex, and return them as
  a sequence of parts that can, in turn be explicit parts and optional parts."
  [s]
  (let [ts (if (and (.startsWith s "(") (.endsWith s ")")) (.subSequence s 1 (dec (count s))) s)]
    (loop [src ts
           prts []]
      (let [idx (loop [i 0
                       lvl 0]
                  (case (nth src i nil)
                    (\N \E \S \W) (recur (inc i) lvl)
                    \( (recur (inc i) (inc lvl))
                    \) (recur (inc i) (dec lvl))
                    \| (if (zero? lvl) i (recur (inc i) lvl))
                    nil -1))]
        (if-not (neg? idx)
          (let [hdr (if (pos? idx) (subs src 0 idx))]
            (recur (subs src (inc idx)) (conj prts hdr)))
          (conj prts src))))))

(defn breakdown
  ""
  [s]
  (let [opt? (fn [s] (and (.startsWith s "(") (.endsWith s ")")))
        dir? (fn [s] (neg? (.indexOf s "|")))]
    (cond
      (dir? s) s
      (opt? s) (vec (map breakdown (fork s)))
      :else    (map breakdown (chop s)))))

; (defn walk-it
;   ""
;   [s]
;   (let [loc (atom [0 0])
;         walls (atom [])
;         mv-n (fn [[x y]] (reset! loc [(first @loc) (dec (second @loc))]))
;         mv-s (fn [[x y]] (reset! loc [(first @loc) (inc (second @loc))]))
;         mv-e (fn [[x y]] (reset! loc [(inc (first @loc)) (second @loc)]))
;         mv-w (fn [[x y]] (reset! loc [(dec (first @loc)) (second @loc)]))
;         mvfn {\N mv-n \S mv-s \E mv-e \W mv-w}
;         ]
;     (for [steps s]
;       (if-let [wlk (first steps)]
;         (cond
;           (string? wlk)
;             (doseq [stp wlk]
;               (case stp
;                 \N (let [[x y] @loc
;                         ]
;                      (swap! walls conj [])
;                     )
;                 \S
;                 \E
;                 \W
;                 )
;               )
;           (vector? wlk)
;             nil
;         )
;         walls
;         )
;       ))

(defn show
  ""
  [s]
  nil
  )

(defn one
  ""
  []
  nil)

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src ["^ENWWW(NEEE|SSE(EE|N))$"
             "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
             "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
             "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
            ]
      ]
    (breakdown (second src))
    ; {:src (first src)
    ;  :path (breakdown (first src))}
  ))
