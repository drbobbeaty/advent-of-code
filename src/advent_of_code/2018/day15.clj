(ns advent-of-code.2018.day15
  "Fourteenth day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.util :refer [split]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]
            [clojure.walk :as cw]))

(defn scrape
  "Function to take a sequence of strings - like the maps of the cave, and
  turn it into a map where the keys are the [row col] and the value is the
  non-'.' character. There's no need to have the spaces in the data."
  [s]
  (into {} (for [[r rs] (map vector (range) s)
                 [c mc] (map vector (range) rs)
                 :when (not= \. mc)]
             [[r c] mc])))

(def trial1
  "This is the test map for part of the mechanics of the battle."
  (scrape ["#######"
           "#E..G.#"
           "#...#.#"
           "#.G.#G#"
           "#######"]))

(def puzzle
  "This is the input of the location of the battle royale area."
  (-> (slurp "resources/2018/input/day15.txt")
    (cs/trim)
    (split "\n")
    (scrape)))

(defn show
  "Function to take a map of the positions and characters to make a human-
  readable version of the map."
  [brd]
  (let [rmax (apply max (distinct (map first (keys brd))))
        cmax (apply max (distinct (map second (keys brd))))]
    (for [r (range (inc rmax))]
      (apply str (for [c (range (inc cmax))] (get brd [r c] \.))))))

(defn turns
  "Function to look at the data for a map, and return a sequence of units that
  need to take a turn, and in the order they need to take their turn. This is
  set up by the statement of the problem."
  [brd]
  (let [bb (for [[p c] brd :when (#{\E \G} c)] [p c])]
    (sort-by (comp (juxt first second) first) bb)))

(defn bfs
  "Function to complete a breadth-first search (BFS) of the board with the
  set of wall pieces, and the target locations of the 'bad guys'. The starting
  position is given as well."
  [brd wall tgts [px py]]
  (let [bts (keys brd)
        rmax (if (empty? bts) 0 (apply max (map second bts)))
        cmax (if (empty? bts) 0 (apply max (map first bts)))
        ; oob? (fn [[x y]] (or (neg? x) (< cmax x) (neg? y) (< rmax y)))
        ; updn (fn [p] (if (oob? p) -1 1))
        ; fin [tx ty 0]
        hits (atom [])
        ]
    (loop [pts [[px py 0]]        ;; [x y len]
           visit (set [[px py]])  ;; [x y]
           eps (set tgts)
           ]
      (if-let [[x y len] (first pts)]
        (if (eps [x y])
          len
          (let [mov (for [p [[x (inc y)] [x (dec y)] [(dec x) y] [(inc x) y]]
                          :let [p' (if (and (zero? lvl) (oob? p)) p (get warp p p))
                                nl (if (and rec (not= p p')) (+ lvl (updn p)) lvl)]
                          :when (not (or (nil? (brd p')) (wall (brd p')) (oob? p')))
                          :let [ps (concat p' [nl])]
                          :when (not (visit ps))]
                      ps)]
            (recur
              (concat (rest pts) (map (fn [[x y nl]] [x y nl (inc len)]) mov))
              (union visit (set mov)))))))))

(defn step
  ""
  [[p c] brd]
  (let [enemy? (fn [bc] (and (#{\E \G} bc) (not= c bc)))
        bad (for [[bp bc] brd :when (enemy? bc)] bp)
       ]
  )
  )

; (defn bget
;   "Function to get the value of the battlefield at column `x`, and row
;   `y`. If nothing is there, a character space is returned - just to make
;   the comparisons easier."
;   [bf x y]
;   (-> (nth bf y [])
;     (nth x \space)))

; (defn overlay
;   "Function to overlay the players on the battlefield so that we can get
;   a complete picture of what's where, and how the state of the battle is."
;   [bf ap]
;   (loop [bfr bf
;          ps ap]
;     (if-let [{c :char [x y] :pos} (first ps)]
;       (recur (->> (vec (assoc (nth bfr y []) x c))
;                (assoc (vec bfr) y)
;                (vec))
;              (rest ps))
;       bfr)))

; (defn open?
;   "Predicate function to take a battlefield, a sequence of players, and
;   a location in the battlefield, and returns `true` if the space is open,
;   meaning not a wall or a player."
;   ([bf x y]
;     (#{\. \space} (bget bf x y)))
;   ([bf ps x y]
;     (open? (overlay bf ps) x y)))

; (defn findp
;   "Function to extract all the elves and goblins on the battlefield and
;   for each, return a map of their location - this is how we will track
;   each of them."
;   [bf]
;   (let [war #{\E \G}]
;     (for [[y row] (map vector (range) bf)
;           [x c] (map vector (range) row)
;           :when (war c)]
;       {:char c :pos [x y] :hp 200 :pwr 3})))

; (defn sortp
;   "Function to sort the players by their location in the battlefield, so
;   that we are ready to move each towards opponents, attack, etc."
;   [sp]
;   (sort-by (comp (juxt second first) :pos) sp))

; (defn clearp
;   "Function to remove all the elves and goblins from the battfield map so
;   that it's ready to be the 'back-drop' for the movement of the players.
;   This returns the battlefield map with all the players replaced with the
;   empty place marker - '.'"
;   [bf]
;   (map #(cw/postwalk-replace {\E \. \G \.} %) bf))

; (defn pretty
;   "Function to make a pretty view of the battlefield for outputting."
;   [bf]
;   (let [fmt (fn [s] (format "%s" (apply str s)))]
;     (cs/join "\n" (map fmt bf))))

; (defn blog
;   "Function to make a nice log message for the provided battlefield. This
;   will make it easy to see what's happening."
;   [bf & [msg]]
;   (info
;     (if msg
;       (str msg "\n" (pretty bf))
;       (str "battlefield:\n" (pretty bf)))))

(defn one
  ""
  [& [n]]
  nil)

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (step (first (turns trial1))))
