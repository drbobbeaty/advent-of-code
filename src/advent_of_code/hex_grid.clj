;;
;; For this namespace, we will look at the hex grid, show below, where the
;; y-axis lines up nicely, but the x-axis is really 45 deg off the y-axis.
;;

;;                    y                  x
;;                    |                 /
;;       \    /    \    /    \    /    \    /
;;        +--+      +--+      +--+  3,0 +--+
;;       /    \    /    \    /    \    /    \
;;    --+ -2,2 +--+  0,1 +--+  2,0 +--+      +--
;;       \    /    \    /    \    /    \    /
;;        +--+ -1,1 +--+  1,0 +--+      +--+
;;       /    \    /    \    /    \    /    \
;;    --+      +--+  0,0 +--+      +--+      +--
;;       \    /    \    /    \    /    \    /
;;        +--+ -1,0 +--+ 1,-1 +--+      +--+
;;       /    \    /    \    /    \    /    \
;;    --+ -2,0 +--+ 0,-1 +--+ 2,-2 +--+      +--
;;       \    /    \    /    \    /    \    /
;;        +--+      +--+      +--+      +--+
;;       /    \    /    \    /    \    /    \
;;

(ns advent-of-code.hex-grid
  "Namespage for functions to do with a hex-grid that has come up in probelms
  and there is a simple way to represent it."
  (require [advent-of-code.util :refer [sign lcase]]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(defn move
  "Function to move one step, or a series of steps on the hex grid, starting
  at the supplied point, and moving through the grid as instructe to by the
  steps."
  [pos step]
  (cond
    (string? step) (move pos [step])
    (coll? step)   (loop [[x y] pos
                          stps step]
                     (if-let [stp (first stps)]
                       (case (lcase stp)
                         "ne" (recur [(inc x) y] (rest stps))
                         "se" (recur [(inc x) (dec y)] (rest stps))
                         "s"  (recur [x (dec y)] (rest stps))
                         "sw" (recur [(dec x) y] (rest stps))
                         "nw" (recur [(dec x) (inc y)] (rest stps))
                         "n"  (recur [x (inc y)] (rest stps)))
                       [x y]))
    :else pos))

(defn dist
  "Function to compute the hex-Manhattan distance between the two supplied
  points. This is very much based on the hex structure of the map, but if
  that scheme is adhered to, this works just fine."
  [[a b] [c d]]
  (let [dx (- c a)
        dy (- d b)]
    (if (= (sign dx) (sign dy))
      (Math/abs (+ dx dy))
      (max (Math/abs dx) (Math/abs dy)))))
