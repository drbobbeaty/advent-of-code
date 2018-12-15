(ns advent-of-code.2018.day15
  "Fourteenth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]
           [clojure.walk :as cw]))

(def puzzle
  "This is the input of the location of the battle royale area."
  (-> (slurp "resources/2018/input/day15.txt")
      (cs/trim)
      (cs/split #"\n")
      (as-> s (map vec s))))

(defn cap
  "Function to cap the value x at one of the two limits, if it's out of range.
  This is very useful in many contexts to limit the excursion of the value."
  [lo x hi]
  (cond
    (< x lo) lo
    (< hi x) hi
    :else    x))

(defn add
  "Simple function to add the two ordered-pairs and return an ordered-pair."
  [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn dist
  "Simple function to determine the the Manhattan distance between the two
  ordered-pairs and return an integer."
  [[a b] [c d]]
  (+ (Math/abs (- a c)) (Math/abs (- b d))))

(defn bget
  "Function to get the value of the battlefield at column `x`, and row
  `y`. If nothing is there, a character space is returned - just to make
  the comparisons easier."
  [bf x y]
  (-> (nth bf y [])
      (nth x \space)))

(defn overlay
  "Function to overlay the players on the battlefield so that we can get
  a complete picture of what's where, and how the state of the battle is."
  [bf ap]
  (loop [bfr bf
         ps ap]
    (if-let [{c :char [x y] :pos} (first ps)]
      (recur (->> (vec (assoc (nth bfr y []) x c))
               (assoc (vec bfr) y)
               (vec))
             (rest ps))
      bfr)))

(defn open?
  "Predicate function to take a battlefield, a sequence of players, and
  a location in the battlefield, and returns `true` if the space is open,
  meaning not a wall or a player."
  ([bf x y]
    (#{\. \space} (bget bf x y)))
  ([bf ps x y]
    (open? (overlay bf ps) x y)))

(defn bpath
  "Predicate function to determine if there is a path from the first point
  to the second point without something being in the way. This is a nasty
  search problem as it might be a horribly complex search path."
  ([bf sp ep]
    (loop [[x y] sp
           pp []]
      (if (= [x y] ep)
        pp
        (let [x? (< y (second ep))
              [dx dy] (if (< y (second ep))
                        (let [cx (cap -1 (- (first ep) x) 1)
                              cy (if (zero? cx) (cap -1 (- (second ep) y) 1) 0)]
                          [cx cy])
                        (let [cy (cap -1 (- (second ep) y) 1)
                              cx (if (zero? cy) (cap -1 (- (first ep) x) 1) 0)]
                          [cx cy]))
              nx (+ x dx)
              ny (+ y dy)]
          (if (open? bf nx ny)
            (recur [nx ny] (conj pp [nx ny])))))))
  ([bf ps sp ep]
    (bpath (overlay bf ps) sp ep)))

(defn findp
  "Function to extract all the elves and goblins on the battlefield and
  for each, return a map of their location - this is how we will track
  each of them."
  [bf]
  (let [war #{\E \G}]
    (for [[y row] (map vector (range) bf)
          [x c] (map vector (range) row)
          :when (war c)]
      {:char c :pos [x y] :hp 200 :pwr 3})))

(defn sortp
  "Function to sort the players by their location in the battlefield, so
  that we are ready to move each towards opponents, attack, etc."
  [sp]
  (sort-by (comp (juxt second first) :pos) sp))

(defn clearp
  "Function to remove all the elves and goblins from the battfield map so
  that it's ready to be the 'back-drop' for the movement of the players.
  This returns the battlefield map with all the players replaced with the
  empty place marker - '.'"
  [bf]
  (map #(cw/postwalk-replace {\E \. \G \.} %) bf))

(defn pretty
  "Function to make a pretty view of the battlefield for outputting."
  [bf]
  (let [fmt (fn [s] (format "%s" (apply str s)))]
    (cs/join "\n" (map fmt bf))))

(defn blog
  "Function to make a nice log message for the provided battlefield. This
  will make it easy to see what's happening."
  [bf & [msg]]
  (info
    (if msg
      (str msg "\n" (pretty bf))
      (str "battlefield:\n" (pretty bf)))))

(defn best-move
  "Function to return the best move, as described by the rules in the puzzle,
  for a given player, amongst a set of players, on a battlefield. The returned
  value is simply a tuple [x y]."
  [p ps bf]
  (let [ops (remove #(= (:pos %) (:pos p)) ps)
        bfv (overlay bf ops)
        poss (for [b (filter #(not= (:char %) (:char p)) ps)
                   :let [bp (:pos b)]
                   chk [[-1 0] [0 -1] [1 0] [0 1]]
                   :let [[bx by] (add (:pos b) chk)]
                   :when (open? bfv bx by)
                   :let [bp (bpath bfv (:pos p) [bx by])]
                   :when bp]
               {:pos [bx by] :path bp})]
    (if (pos? (count poss))
      (let [shrt (apply min (map #(count (:path %)) poss))]
        (->> (filter #(= shrt (count (:path %))) poss)
          (sort-by (comp (juxt second first) :pos))
          (first)
          (:path)
          (first))))))

(defn attack
  "Function to return the list of locations where a neighboring enemy is
  located, and so you can attack."
  [p ps bf]
  (for [chk [[-1 0] [0 -1] [1 0] [0 1]]
        :let [bp (add (:pos p) chk)]
        :when (some #(and (not= (:char %) (:char p)) (= bp (:pos %))) ps)
        ]
    bp))

(defn turn
  ""
  [bf ply]
  (let [mv (fn [p & ps] (if-let [np (best-move p (flatten ps) bf)] (assoc p :pos np) p))
       ]
    (loop [todo (sortp ply)
           done []]
      (if-let [c (first todo)]
        (let [atk (attack c ply bf)
              c' (if (empty? atk) (mv c done (rest todo)) c)
            ]
          (blog (overlay bf (concat done (rest todo))))
          (recur (rest todo) (conj done c'))
          )
        done))))

(defn one
  ""
  [& [n]]
  nil)

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src (map vec ["#######"
                      "#...G.#"
                      "#..G.G#"
                      "#.#.#G#"
                      "#...#E#"
                      "#.....#"
                      "#######"])
        fs (findp src)
        bf (clearp src)
        tp (partial turn bf)
        ; big-e (first (filter #(= \E (:char %)) fs))
        ]
    (blog (overlay bf (tp fs)))
  ))
