(ns advent-of-code.2018.day17
  "Seventeenth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the scan into the ground with sand and clay."
  (->> "resources/2018/input/day17.txt"
    (io/reader)
    (line-seq)))

(def sample
  "This is the sample scan in the puzzle statement."
  ["x=495, y=2..7"
   "y=7, x=495..501"
   "x=501, y=3..7"
   "x=498, y=2..4"
   "x=506, y=1..2"
   "x=498, y=10..13"
   "x=504, y=10..13"
   "y=13, x=498..504"])

(defn code-it
  "Function to take a string, or a sequence of strings, and convert them into
  a sequence of order pairs that can then be used to identify the clay deposits
  in the scan. This will then be used to convert the input into a sequence of
  updates to the scan so that we can track the water flow."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))]
    (cond
      (string? s) (cond
                    (.startsWith s "x=")
                      (let [[x yl yh] (->> (drop 1 (re-matches #"^x=(\d+), y=(\d+)..(\d+)$" s))
                                        (map pint))]
                        (map vector (repeat x) (range yl (inc yh))))
                    (.startsWith s "y=")
                      (let [[y xl xh] (->> (drop 1 (re-matches #"^y=(\d+), x=(\d+)..(\d+)$" s))
                                        (map pint))]
                        (map vector (range xl (inc xh)) (repeat y))))
      (coll? s)   (apply concat (map code-it s))
      :else       s)))

(def scan (atom []))
(def xoffset (atom 0))
(def xlimit (atom 0))
(def ylow (atom 0))
(def ylimit (atom 0))

(def stopper (atom 0))

(defn sget
  "Function to get the value of the scan at `y` (row), and `x` (column). If
  nothing is there, 'sand' ('.') is returned."
  [x y]
  (-> (nth @scan y [])
      (nth (- x @xoffset) \.)))

(defn sset
  "Function to set the scan pixel at `y` (row), and `x` (column), to the
  value `v`."
  [x y v]
  (reset! scan (-> (nth @scan y [])
                      (assoc (- x @xoffset) v)
                      (->> (assoc @scan y)))))

(defn build
  "Function to create the 'scan' with the provided scan data. This
  will destroy the scan that is already there, and the new scan will be
  set to the data provided."
  [s]
  (let [clay (code-it s)
        [xl xh] (apply (juxt min max) (map first clay))
        [yl yh] (apply (juxt min max) (map second clay))
        xos (- xl 20)
        row (vec (repeat (+ 40 (- xh xl)) \.))]
    (reset! xoffset xos)
    (reset! xlimit (+ 20 xh))
    (reset! ylow yl)
    (reset! ylimit yh)
    (reset! scan (vec (repeat (inc yh) row)))
    (sset 500 0 \+)
    (doseq [[x y] clay]
      (sset x y \#))))

(defn show
  "Function to simply make a decent looking output for the 'scan' based on
  the state of that scan in the internal logic of the code."
  []
  (doseq [r @scan]
    (println (apply str r))))

(defn slog
  "Function to make a nice log message for the provided scan. This
  will make it easy to see what's happening."
  []
  (let [fmt (fn [[i s]] (format "[%4d] %s" i (apply str s)))]
    (info (str "scan:\n" (cs/join "\n" (map fmt (map vector (range) @scan)))))))

(defn wet
  "Function to count the number of wet cells in the scan - they could be
  water (~) or they counld have once had water (|)."
  []
  (apply + (map #(count (filter #{\~ \|} %)) (drop @ylow @scan))))

(defn step
  ""
  [[x y] dir]
  (case dir
    :down  [x (inc y)]
    :right [(inc x) y]
    :left  [(dec x) y]))

(declare run drip)

(defn edge
  "Function to find the edge, starting from the point, and moving in the
  incremental direction provided, we look for a wall, or a cliff, and return
  the x value and condition of that junction."
  [sx sy dx]
  (loop [x sx]
    (if (<= @xoffset x @xlimit)
      (let [[nx ny] [(+ dx x) sy]
            nv (sget nx ny)
            nvd (sget nx (inc ny))
            ans (cond
                  (= \# nv) [x :wall]
                  (and (#{\. \|} nv) (#{\~ \#} nvd)) nil
                  (and (#{\. \|} nv) (#{\. \|} nvd)) [nx :spill])]
        (if (nil? ans)
          (recur nx)
          ans))
      [x :edge])))

(defn flow
  ""
  [sx sy]
  (let [[rx rb] (edge sx sy 1)
        [lx lb] (edge sx sy -1)]
    (cond
      (= :wall rb lb)
        (doseq [x' (range lx (inc rx))] (sset x' sy \~))
      (or (= :spill rb) (= :spill lb))
        (do
          (doseq [x' (range lx (inc rx))] (sset x' sy \|))
          (if (= :spill rb) (drip rx sy))
          (if (= :spill lb) (drip lx sy))
        )
      (or (= :edge rb) (= :edge lb))
        false)
    false))

(defn drip
  ""
  [& [sx sy]]
  (loop [x (or sx 500)
         y (or sy 0)]
    (if (<= y @ylimit)
      (let [v (sget x y)
            ny (inc y)
            nv (sget x ny)
            go? (case v
                  \+ true
                  \. (case nv
                       (\. \|) (do (sset x y \|) true)
                       (\# \~) (flow x y)
                     )
                  \| (case nv
                       \.      (do (sset x y \|) true)
                       \|      true
                       (\# \~) (flow x y)
                     )
                  )]
        (if go?
          (recur x ny)))
      (do
        (when (zero? (mod @stopper 5000))
          (infof "stopper=%s ... wet=%s" @stopper (wet))
          (slog))
        (swap! stopper inc))
      )))

(defn run
  ""
  [& [sx sy]]
  (loop []
    (let [curr @scan]
      (drip sx sy)
      (if-not (= curr @scan)
        (recur)))))

(defn one
  "Got to 37077 and everything looks right, but AoC says that is too high.
  They have not completely specified the rules, and that's what the issue
  really is."
  []
  (build puzzle)
  (doseq [i (range 200)]
    (reset! stopper 0)
    (drip))
  (slog)
  (wet))

(defn check
  "Function to take the solution we have in text form, and calculate the
  value as it's meant to be. We get 37073, and that's exactly right."
  []
  (->> "resources/2018/input/day17.ans"
    (io/reader)
    (line-seq)
    (drop-while (fn [s] (zero? (count (filter #(= \# %) s)))))
    (map (fn [s] (count (filter #{\| \~} s))))
    (apply +)))

(defn puddles
  "Function to take the solution we have in text form, and calculate the
  value of the standing water."
  []
  (->> "resources/2018/input/day17.ans"
    (io/reader)
    (line-seq)
    (map (fn [s] (count (filter #(= \~ %) s))))
    (apply +)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [x 5]
    (build puzzle)
    (run)
    (slog)
    (wet)))
