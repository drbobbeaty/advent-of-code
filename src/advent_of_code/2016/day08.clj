(ns advent-of-code.2016.day08
  "Eighth day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(declare rect rotate)

(defn code-it
  "Function to do the simple parsing of the input from the AoC site. Just
  makes sense to provide some structure to the data so that it's easier to
  work with."
  [s]
  (let [pint (fn [x] (Integer/parseInt x))]
    (cond
      (string? s) (cond
                    (.startsWith s "rect")
                      (let [pts (drop 1 (re-matches #"^rect (\d+)x(\d+)$" s))]
                        {:action rect
                         :w (pint (first pts))
                         :h (pint (second pts))})
                    (.startsWith s "rotate")
                      (let [pts (drop 1 (re-matches #"^rotate (.+) (.)=(\d+) by (\d+)$" s))]
                        {:action rotate
                         :axis (nth pts 0 nil)
                         :index (pint (nth pts 2))
                         :amount (pint (nth pts 3))}))
      (coll? s)   (map code-it s)
      :else       s)))

(def puzzle
  "This is the input from the code for the Easter Bunny's keypad display. The
  result of this will be a sequence of commands."
  (-> (slurp "resources/2016/input/day08.txt")
      (cs/trim)
      (cs/split #"\n")
      (code-it)))

(def display (atom []))

(defn build
  "Function to create the 'display' with the provided width and height. This
  will destroy the display that is already there, and the new display will be
  reset to all 'off'."
  [w h]
  (let [row (vec (repeat w 0))]
    (reset! display (vec (repeat h row)))))

(defn dget
  "Function to get the value of the pixel in the display at row `r`, and column
  `c`."
  [r c]
  (-> (nth @display r [])
      (nth c nil)))

(defn dset
  "Function to set the pixel at row `r`, and column `c`, to the value `v`."
  [r c v]
  (reset! display (-> (nth @display r [])
                      (assoc c v)
                      (->> (assoc @display r)))))

(defn rect
  "Function to execute the 'rect' command on the 'display' with the parameters
  for the call in the provided map."
  [{w :w h :h}]
  (doseq [x (range w)
          y (range h)]
    (dset y x 1)))

(defn rotate-col
  "Function to rotate the specified column index one display bit down. This
  will then be used to build up the more general `rotate` function, below."
  [idx]
  (let [rows (count @display)
        lg (dget (dec rows) idx)]
    (doseq [r (range (dec rows) 0 -1)]
      (dset r idx (dget (dec r) idx)))
    (dset 0 idx lg)))

(defn rotate-row
  "Function to rotate the specified row index one display bit right. This
  will then be used to build up the more general `rotate` function, below."
  [idx]
  (let [cols (count (first @display))
        lg (dget idx (dec cols))]
    (doseq [c (range (dec cols) 0 -1)]
      (dset idx c (dget idx (dec c))))
    (dset idx 0 lg)))

(defn rotate
  "Function to execute the 'rotate' command on the 'display' with the parameters
  for the call in the provided map."
  [{ax :axis idx :index amt :amount}]
  (cond
    (= "column" ax) (doseq [s (range amt)] (rotate-col idx))
    (= "row" ax)    (doseq [s (range amt)] (rotate-row idx))
    :else           nil))

(defn cnt-pixels
  "Function to count the pixels in the 'display' that have the state provided -
  `true` for 'on' and `false` for 'off'. This uses the existing display in this
  namespace."
  [state]
  (let [ff (if state pos? zero?)
        rcnt (fn [s] (count (filter ff s)))]
    (->> (map rcnt @display)
         (apply +))))

(defn show
  "Function to simply make a decent looking output for the 'display' based on
  the state of that display in the internal logic of the code."
  []
  (doseq [r @display]
    (println (apply str (map #(if (pos? %) "#" ".") r)))))

(defn one
  "Function to take all the commands from the `puzzle` and run them over a 50x6
  display and then count up the number of pixels that are 'lit'."
  [& [cmds]]
  (build 50 6)
  (doseq [c (or cmds puzzle)
          :let [dc (:action c)]]
    (dc c))
  (cnt-pixels true))

(defn two
  "Function to take all the commands from the `puzzle` and run them over a 50x6
  display and then display it to see what it's trying to say to us."
  [& [cmds]]
  (build 50 6)
  (doseq [c (or cmds puzzle)
          :let [dc (:action c)]]
    (dc c))
  (show))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (build 7 3)
  (let [src ["rect 3x2" "rotate column x=1 by 1" "rotate row y=0 by 4" "rotate column x=1 by 1"]]
    (doseq [c (code-it src)
            :let [dc (:action c)]]
      (dc c))
    (show)))
