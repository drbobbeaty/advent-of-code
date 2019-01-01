(ns advent-of-code.2018.day13
  "Thirteenth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]
            [clojure.walk :as cw]))

(def trial
  "This is the test input from the tracks and cars for the elves."
  (-> (slurp "resources/2018/input/day13_trial.txt")
      (cs/split #"\n")
      (as-> s (map vec s))))

(def trial-2
  "This is the second test input from the tracks and cars for the elves."
  (-> (slurp "resources/2018/input/day13_t2.txt")
      (cs/split #"\n")
      (as-> s (map vec s))))

(def puzzle
  "This is the actusal input from the tracks and cars for the elves."
  (-> (slurp "resources/2018/input/day13.txt")
      (cs/split #"\n")
      (as-> s (map vec s))))

(defn tget
  "Function to get the value of the track layout at column `x`, and row
  `y`. If nothing is there, a character space is returned - just to make
  the comparisons easier."
  [trk x y]
  (-> (nth trk y [])
      (nth x \space)))

(defn find-carts
  "Function to extract all the carts on the track and for each, return a
  map of their location, direction, and last turn - this is how we will
  track each of them."
  [trk]
  (let [carts #{\< \> \v \^}]
    (for [[y row] (map vector (range) trk)
          [x c] (map vector (range) row)
          :when (carts c)]
      {:dir c :pos [x y] :turn :left})))

(defn next-turn
  "Convenience function to indicate what direction the cart will turn when
  it next hits an intersection."
  [t]
  (case t
    :left     :straight
    :straight :right
    :left))

(defn sort-carts
  "Function to sort the carts by their location on the tracks, so that we
  are ready to move each one a single unit, and in so doing, complete a tick."
  [sc]
  (sort-by (comp (juxt second first) :pos) sc))

(defn clear-carts
  "Function to remove all the carts from the track map so that it's ready
  to be the 'back-drop' for the movement of the carts. This returns the track
  map with all the carts replaced with the appropriate track segments."
  [trk]
  (map #(cw/postwalk-replace {\> \- \< \- \^ \| \v \|} %) trk))

(defn move-cart
  "Function that takes a single cart, and moves it along the track, as
  directed by the rules."
  [trk {dir :dir [x y] :pos turn :turn :as crt}]
  (let [[x' y'] (case dir
                  \v [x (inc y)]
                  \^ [x (dec y)]
                  \> [(inc x) y]
                  \< [(dec x) y])
        nts (tget trk x' y')
        [d' t'] (case dir
                  \v
                    (case nts
                      \| [dir turn]
                      \\ [\> turn]
                      \/ [\< turn]
                      \+ (case turn
                           :left     [\> (next-turn turn)]
                           :straight [dir (next-turn turn)]
                           :right    [\< (next-turn turn)]))
                  \^
                    (case nts
                      \| [dir turn]
                      \\ [\< turn]
                      \/ [\> turn]
                      \+ (case turn
                           :left     [\< (next-turn turn)]
                           :straight [dir (next-turn turn)]
                           :right    [\> (next-turn turn)]))
                  \>
                    (case nts
                      \- [dir turn]
                      \\ [\v turn]
                      \/ [\^ turn]
                      \+ (case turn
                           :left     [\^ (next-turn turn)]
                           :straight [dir (next-turn turn)]
                           :right    [\v (next-turn turn)]))
                  \<
                    (case nts
                      \- [dir turn]
                      \\ [\^ turn]
                      \/ [\v turn]
                      \+ (case turn
                           :left     [\v (next-turn turn)]
                           :straight [dir (next-turn turn)]
                           :right    [\^ (next-turn turn)])))]
    {:dir d' :pos [x' y'] :turn t'}))

(defn tick
  "Function to run through all the carts, stopping at the first collision,
  and marking that cart's direction with an 'X'. If there are no collisions,
  we will complete a pass of all the carts - top-to-bottom, and left-to-right."
  [track carts]
  (loop [todo (sort-carts carts)
         done []]
    (if-let [c (first todo)]
      (let [c' (move-cart track c)
            hits (into {} (for [[k v] (frequencies (map :pos (concat done [c'] (rest todo))))
                                :when (< 1 v)]
                            [k v]))]
        (if (empty? hits)
          (recur (rest todo) (conj done c'))
          (concat done [(assoc c' :dir \X)] (rest todo))))
      done)))

(defn tock
  "Function to run through all the carts, completing a tick no matter what,
  because at each collision, we remove the two carts in the collision, and
  continue on with moving the rest of the carts. This means that we will
  always complete a tick, but we might finish with less carts than we started."
  [track carts]
  (loop [todo (sort-carts carts)
         done []]
    (if-let [c (first todo)]
      (let [c' (move-cart track c)
            hit (first (for [[k v] (frequencies (map :pos (concat done [c'] (rest todo))))
                             :when (< 1 v)]
                         k))
            todo' (if hit (remove #(= hit (:pos %)) (rest todo)) (rest todo))
            done' (if hit (remove #(= hit (:pos %)) done) (conj done c'))]
        (if (pos? (count todo'))
          (recur todo' done')
          done'))
      done)))

(defn overlay
  "Function to overlay the carts on the track so that we can get a complete
  picture of what's on where, and how the state of the yard is."
  [trk cts]
  (loop [t trk
         cs cts]
    (if-let [{dir :dir [x y] :pos turn :turn} (first cs)]
      (recur (->> (vec (assoc (nth t y []) x dir))
               (assoc (vec t) y)
               (vec))
             (rest cs))
      t)))

(defn pretty
  "Function to make a pretty view of the track layout for outputting."
  [trk]
  (let [fmt (fn [s] (format "%s" (apply str s)))]
    (cs/join "\n" (map fmt trk))))

(defn tlog
  "Function to make a nice log message for the provided track layout. This
  will make it easy to see what's happening."
  [trk & [msg]]
  (info
    (if msg
      (str msg "\n" (pretty trk))
      (str "track:\n" (pretty trk)))))

(defn one
  "Function to complete part 1 - looking for the first collision on the
  track system."
  []
  (let [track (clear-carts puzzle)
        carts (find-carts puzzle)]
    (loop [cts carts]
      (let [ncts (tick track cts)
            hits (filter #(= \X (:dir %)) ncts)]
        (if (empty? hits)
          (recur ncts)
          (first hits))))))

(defn two
  "Function to complete part 2 - looking for the last cart after a complete
  tick that is the only cart left on the tracks. After any collision, both
  effected carts are removed - in `tock`."
  []
  (let [track (clear-carts puzzle)
        carts (find-carts puzzle)]
    (loop [cts carts]
      (let [ncts (tock track cts)]
        (if (< 1 (count ncts))
          (recur ncts)
          (first ncts))))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [track (clear-carts trial-2)
        carts (find-carts trial-2)]
    (loop [cts carts
           cnt 10]
      (let [ncts (tock track cts)]
        (tlog (overlay track ncts))
        (if (and (pos? cnt) (< 1 (count ncts)))
          (recur ncts (dec cnt))
          (first ncts))))))
