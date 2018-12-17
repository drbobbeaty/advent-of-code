(ns advent-of-code.2018.day17
  "Sixteenth day's solutions for the Advent of Code 2018"
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
(def ylimit (atom 0))

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
        xos (- xl 10)
        row (vec (repeat (+ 20 (- xh xl)) \.))]
    (reset! xoffset xos)
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
  (let [fmt (fn [s] (format "%s" (apply str s)))]
    (info (str "scan:\n" (cs/join "\n" (map fmt @scan))))))

(defn wet
  ""
  []
  (let [cnt (atom 0)]
    (doseq [row @scan
            c row
            :when (#{\~ \|} c)]
      (swap! cnt inc))
    @cnt))

(defn step
  ""
  [[x y] dir]
  (case dir
    :down  [x (inc y)]
    :right [(inc x) y]
    :left  [(dec x) y]))

(declare run drip)

(defn flow
  ""
  ([sx sy]
    (infof "flow: x,y = %s, %s" sx sy)
    (let [spc (sget sx sy)
          cr (sget (inc sx) sy)
          cl (sget (dec sx) sy)
          slide (fn [mfn]
                  (loop [x' sx]
                    (let [cr (sget (mfn x') sy)]
                      (cond
                        (= \. cr) (drip x' sy)
                        (= \| cr) (recur (mfn x'))))))
         ]
      ; (if (and (= \| spc) (#{\| \#} cr) (#{\| \#} cl))
      (if (= \| spc cr)
        (loop [x' sx]
          (let [cr (sget (inc x') sy)]
            (cond
              (= \. cr) (drip x' sy)
              (= \| cr) (recur (inc x'))))))
      (if (= \| spc cl)
        (loop [x' sx]
          (let [cl (sget (dec x') sy)]
            (cond
              (= \. cl) (drip x' sy)
              (= \| cl) (recur (dec x'))))))
      (when (and (not= \| cr) (not= \| cl))
        (sset sx sy \.)
        (flow sx sy :right)
        (flow sx sy :left))))
  ([sx sy dir]
    (let [spc (sget sx sy)
          ofc (if (= spc \|) \| \~)]
      (loop [x sx
             y sy]
        (let [v (sget x y)
              dn (sget x (inc y))
              [nx ny] (step [x y] dir)
              go? (cond
                    (= \# v) false
                    (= \~ v) true
                    (#{\. \|} v)
                      (cond
                        (= \. dn) (let [dx (if (= :left dir) 1 -1)]
                                    (loop [x' sx]
                                      (when (= \~ (sget x' y))
                                        (sset x' y \|)
                                        (recur (+ x' dx))))
                                    (doseq [x' (apply range (sort [sx x]))]
                                      (sset x' y \|))
                                    (drip x y))
                        (#{\# \~} dn) (do (sset x y ofc) true)
                      )
                    )]
          (if go? (recur nx ny))
        ))
      )
    false
    ))

(defn drip
  ""
  [& [sx sy]]
  (infof "drip: x,y = %s, %s" sx sy)
  (loop [x (or sx 500)
         y (or sy 0)
         dir :down]
    (if (<= y @ylimit)
      (let [v (sget x y)
            [nx ny] (step [x y] dir)
            nv (sget nx ny)
            go? (cond
                  (= \+ v) true
                  (= \. v) (case nv
                             (\. \|) (do (sset x y \|) true)
                             (\# \~) (flow x y)
                           )
                  (= \| v) (case nv
                             \.      (do (sset x y \|) true)
                             \|      true
                             (\# \~) (flow x y)
                           )
                  )
           ]
        (if go?
          (recur nx ny dir))
      )
    )
  )
  )

(defn run
  ""
  [& [sx sy]]
  (loop []
    (let [curr (wet)]
      (drip sx sy)
      (if-not (= curr (wet))
        (recur))
    )
  ))

(defn one
  ""
  []
  (build puzzle)
  (doseq [i (range 500)]
    (drip))
  ; (run)
  (slog)
  (wet))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [x 5
        ]
    (build sample)
    (run)
    ; (drip)
    ; (drip)
    ; (drip)
    ; (drip)
    ; (drip)
    ; (drip)
    ; (drip)
    ; (drip)
    (slog)
    (wet)))
