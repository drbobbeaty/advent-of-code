(ns advent-of-code.2018.day24
  "Twenty-fourth day's solutions for the Advent of Code 2018"
  (require [advent-of-code.2016.day25 :refer [->int]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the location and radius data for all of the nanobots."
  (read-string (slurp "resources/2018/input/day24.clj")))

(def sample
  "This is the sample nanobot data for testing."
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
  selection."
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
  (loop [sa (pwr-sort att)
         sd dfd
         bps []]
    (if-let [a (first sa)]
      (let [sel (target a sd)]
        (recur (rest sa) (remove #(= sel %) sd) (conj bps [a sel])))
      bps)))

(defn turn
  ""
  [arg]
  (let [imm (atom (mapv #(assoc % :type :immune) (:immune arg)))
        inf (atom (mapv #(assoc % :type :infection) (:infection arg)))
        pull (fn [u] (case (:type u)
                       :immune    (nth @imm (dec (:group u)))
                       :infection (nth @inf (dec (:group u)))))
        push (fn [u] (case (:type u)
                       :immune    (swap! imm assoc (dec (:group u)) u)
                       :infection (swap! inf assoc (dec (:group u)) u)))
        mups (->> (concat (match-ups @imm @inf) (match-ups @inf @imm))
               (sort-by (comp :initiative first) rev))
      ]
    (info "=========================================")
    (doseq [[a d] mups
            :when (and a d)
            :let [ca (pull a)
                  cd (pull d)
                  dmg (damage ca cd)]
            :when (and (pos? dmg) (pos? (:units ca)) (pos? (:units cd)))]
      (infof "attacker: %s %s ... damage: %s ... defender: %s %s ... change: %s" (:type ca) (:group ca) dmg (:type cd) (:group cd) (min (:units cd) (quot dmg (:hit-points cd))))
      (push (assoc cd :units (max 0 (- (:units cd) (quot dmg (:hit-points cd)))))))
    {:immune (map #(dissoc % :type) @imm)
     :infection (map #(dissoc % :type) @inf)}))

(defn one
  ""
  []
  (let [ucf (fn [s] (mapv (juxt :group :units) (filter #(pos? (:units %)) s)))
        eg (loop [game puzzle]
             (let [nxt (turn game)
                   cgg (count (filter #(pos? (:units %)) (:immune nxt)))
                   cbg (count (filter #(pos? (:units %)) (:infection nxt)))]
               (if (and (pos? cgg) (pos? cbg))
                 (recur nxt)
                 nxt)))]
    {:immune (ucf (:immune eg))
     :infection (ucf (:infection eg))
     :winner (apply + (map :units (concat (:immune eg) (:infection eg))))}))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [ucf (fn [s] (mapv (juxt :group :units) (filter #(pos? (:units %)) s)))
        eg (loop [game sample]
             (let [nxt (turn game)
                   cgg (count (filter #(pos? (:units %)) (:immune nxt)))
                   cbg (count (filter #(pos? (:units %)) (:infection nxt)))]
               (if (and (pos? cgg) (pos? cbg))
                 (recur nxt)
                 nxt)))]
    {:immune (ucf (:immune eg))
     :infection (ucf (:infection eg))
     :winner (apply + (map :units (concat (:immune eg) (:infection eg))))}))
