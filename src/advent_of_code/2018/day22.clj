(ns advent-of-code.2018.day22
  "Twenty-second day's solutions for the Advent of Code 2018"
  (require [advent-of-code.path :refer [gget dist search]]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the structure of the cave for the puzzle today."
  {:depth 8103, :target [9 758]})

(def sample
  "This is the structure of the cave for the sample in the puzzle."
  {:depth 510 :target [10 10]})

(defn build
  "Function to build up the cave a little bit past the target spot. This is
  all deterministic from the depth and target of the cave."
  [{dp :depth [tx ty] :target :as arg}]
  (let [cv (atom [])
        gi (fn [x y] (cond
                       (= 0 x y) 0
                       (= [x y] [tx ty]) 0
                       (zero? y) (* x 16807)
                       (zero? x) (* y 48271)
                       :else (* (gget @cv (dec x) y) (gget @cv x (dec y)))))
        el (fn [x y] (mod (+ (gi x y) dp) 20183))
        rt (fn [x] (case (mod x 3)
                     0 \.
                     1 \=
                     2 \|))]
    (let [row (vec (for [x (range (+ 6 tx))] (el x 0)))]
      (swap! cv conj row))
    (doseq [y (range 1 (+ 6 ty))
            :let [row (atom [(el 0 y)])]]
      (doseq [x (range 1 (+ 6 tx))
              :let [lgi (if (= [x y] [tx ty]) 0 (* (last @row) (gget @cv x (dec y))))]]
        (swap! row conj (mod (+ lgi dp) 20183)))
      (swap! cv conj @row))
    (mapv (fn [row] (mapv rt row)) @cv)))

(defn risk
  "Function to calculate the risk of the provided cave from the origin to
  the target location."
  [{dp :depth [tx ty] :target :as arg} cave]
  (let [sv (fn [c] (case c
                     \. 0
                     \= 1
                     \| 2))
        cnt (fn [s] (apply + (map sv (take (inc tx) s))))]
    (apply + (map cnt (take (inc ty) cave)))))

(defn cost
  "Function to compute a simple cost function for the A* path - it's just
  the distance from the start to the current, and the current to the end,
  and this works for planty of cases, so let's have it around for others
  to use - if they need it."
  [curr start end]
  (let [g (dist start curr)
        h (dist curr end)
        f (+ g h)]
    [f g h]))

(defn one
  "Function to find the risk in the cave, based on the puzzle structure,
  from the cave entrace to the target location."
  []
  (risk puzzle (build puzzle)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (search (build sample) [0 0] (:target sample)))
