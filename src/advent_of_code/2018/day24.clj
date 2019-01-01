(ns advent-of-code.2018.day24
  "Twenty-fourth day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.2016.day25 :refer [->int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the data structure for the puzzle input for the immune system and
  infection groups."
  (read-string (slurp "resources/2018/input/day24.clj")))

(def sample
  "This is the data structure for the example input for the immune system and
  infection groups."
  (read-string (slurp "resources/2018/input/day24_sample.clj")))

(defn epower
  "Function to compute the effective power of the provided unit."
  [u]
  (* (:units u) (:attack-points u)))

(defn rev
  "Function to do a simple reverse sort versus `compare`. This is for a simple
  descending sort order - nothing special."
  [a b]
  (* -1 (compare a b)))

(defn pwr-sort
  "Function to sort the units based on their effective power rating, and if
  that's equal, then their initiative. This is how they want us to look at
  the sequencing of the selection plans."
  [us]
  (sort-by (juxt epower :initiative) rev us))

(defn damage
  "Function to calculate the damage an attacker will do to a defender based
  on the effective power and the immunities and weaknesses of the defender."
  [a d]
  (let [apwr (epower a)
        imu (:immune d)
        wek (:weak d)]
    (cond
      (and imu (imu (:attack-type a))) 0
      (and wek (wek (:attack-type a))) (* 2 apwr)
      :else                            apwr)))

(defn target
  "Function to return the best target defender for the provided attacker.
  This obeys the rules in the puzzle for the strategy of the target
  selection. If we can do no damage to these defenders, then return a
  `nil` - as opposed to a zero-damage defender."
  [a sd]
  (if (and a (pos? (:units a)))
    (let [dmg (partial damage a)]
      (->> (remove #(zero? (dmg %)) sd)
        (sort-by (juxt dmg epower :initiative) rev)
        (first)))))

(defn match-ups
  "Function to return a sequence of tuples where the first is the attacker,
  and the second is the defender, or `nil`, and these are the battles for
  this one 'direction' of the turn."
  [att dfd]
  (loop [sa (pwr-sort (filter #(pos? (:units %)) att))
         sd (filter #(pos? (:units %)) dfd)
         bps []]
    (if-let [a (first sa)]
      (let [sel (target a sd)]
        (recur (rest sa) (remove #(= sel %) sd) (conj bps [a sel])))
      bps)))

(defn turn
  "Function to take one 'battle' turn, where targets are selected, and then
  those individual skirmishes take place, updating the status of the units
  as they progress, and then finishing with the number of little guys lost
  in this round."
  [arg]
  (let [imm (atom (mapv #(assoc % :type :immune) (:immune arg)))
        inf (atom (mapv #(assoc % :type :infection) (:infection arg)))
        pull (fn [u] (case (:type u)
                       :immune    (nth @imm (dec (:group u)))
                       :infection (nth @inf (dec (:group u)))))
        push (fn [u] (case (:type u)
                       :immune    (swap! imm assoc (dec (:group u)) u)
                       :infection (swap! inf assoc (dec (:group u)) u)))
        lost (atom 0)
        mups (->> (concat (match-ups @imm @inf) (match-ups @inf @imm))
               (sort-by (comp :initiative first) >))
      ]
    (debug "=========================================")
    (doseq [[a d] mups
            :when (and a d)
            :let [ca (pull a)
                  cd (pull d)
                  dmg (damage ca cd)]
            :when (and (pos? dmg) (pos? (:units cd)))
            :let [kc (min (:units cd) (quot dmg (:hit-points cd)))]]
      (debugf "%s %s |%s| does: %s dmg to %s %s (%s), %s-%s = %su" (:type ca) (:group ca) (:initiative ca) dmg (:type cd) (:group cd) (epower cd) (:units cd) kc (- (:units cd) kc))
      (swap! lost + kc)
      (push (update cd :units - kc)))
    {:immune (map #(dissoc % :type) @imm)
     :infection (map #(dissoc % :type) @inf)
     :lost @lost}))

(defn battle
  "Function to start with two armies, and battle until one is gone, or there
  is no more ability to determine a winner - stalemate."
  [m]
  (let [ucf (fn [s] (mapv (juxt :group :units) (filter #(pos? (:units %)) s)))
        eg (loop [game m]
             (let [nxt (turn game)
                   lc (:lost nxt)
                   cgg (count (filter #(pos? (:units %)) (:immune nxt)))
                   cbg (count (filter #(pos? (:units %)) (:infection nxt)))]
              (if (and (pos? lc) (pos? cgg) (pos? cbg))
                (recur nxt)
                nxt)))]
    {:immune (not-empty (ucf (:immune eg)))
     :infection (not-empty (ucf (:infection eg)))
     :remaining (apply + (map :units (concat (:immune eg) (:infection eg))))
     :last-round (:lost eg)}))

(defn one
  "Function to try and see what happens in the little fella's body so I can
  tell the Big Guy what's happening."
  []
  (battle puzzle))

(defn two
  "Function to run the battle with a provided boost to the immune system.
  The answer is the number of *remaining* units on the immune side when the
  battle is won with the minimum boost possible. For me, that's 884."
  [boost]
  (->> (for [u (:immune puzzle)] (update u :attack-points + boost))
    (vec)
    (assoc puzzle :immune)
    (battle)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (->> (for [u (:immune sample)] (update u :attack-points + 1570))
    (vec)
    (assoc sample :immune)
    (battle)))
