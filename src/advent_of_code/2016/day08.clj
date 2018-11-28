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
  willl destroy the display that is already there, and the new display will be
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

(defn rotate
  ""
  [{ax :axis idx :index amt :amount}]
  (cond
    (= "column" ax)
    (= "row" ax)
    ))

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
  ""
  []
  (doseq [r @display]
    (println (apply str (map #(if (pos? %) "#" ".") r)))
    ))

(defn one
  "Function to take all the commands from the `puzzle` and run them over a 50x6
  display and then count up the number of pixels that are 'lit'."
  [cmds]
  (build 50 6)
  (doseq [c cmds
          :let [dc (:action c)]]
    (dc c))
  (cnt-pixels true))

(defn yoyo
  ""
  []
  (build 7 3)
  (let [src ["rect 3x2" "rotate column x=1 by 1" "rotate row y=0 by 4" "rotate column x=1 by 1"]]
    (doseq [c (code-it src)
            :let [dc (:action c)]]
      (dc c))
    (show)
    ))
