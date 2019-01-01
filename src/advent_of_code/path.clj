(ns advent-of-code.path
  "These functions are all about working the A* path-finding algorithm in order
  to move around the grids that we often run into in the AoC daily puzzles. This
  namespace will be used in several others, and is here just for re-use. This
  is all based on the article at:
    https://nakkaya.com/2010/06/01/path-finding-using-astar-in-clojure/"
  (:require [clojure.data.priority-map :refer [priority-map-by]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn dist
  "Function to compute the Manhattan distance between the two points
  provided. This is just the simple distance in the coordinates. These
  coordinates can be 2D or 3D - or really just any two vectors of equal
  length with numerical values. It's pretty flexible."
  [a b]
  (let [d (fn [[i j]] (Math/abs (- i j)))]
    (->> (map vector a b)
      (map d)
      (apply +))))

(defn gget
  "Function to get the value of the of the array at column `x`, and row
  `y` - where the vector-of-vectors is stored in row-major format.. If
  nothing is there, a `nil` is returned - just to make testing easier."
  [vov x y]
  (-> (nth vov y [])
      (nth x nil)))

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

(defn edges
  "For each node we take from the open list, we need to build a list of
  nodes around it. We filter them by checking if the node contains a 1
  in its place on the grid which means we can't go over it or it is
  already in the closed list which means we have already looked at it.
  This is using Manhattan movement - up, down, left, right - as that's
  what AoC typically does. If there is a need for something else, then
  the code needs to include the commented section, and not the other."
  ([grid closed [x y]]
    (let [w (dec (count (first grid)))
          h (dec (count grid))]
      (edges grid w h closed [x y])))
  ([grid width height closed [x y]]
    (for [[tx ty] (map vector (map #(+ x %) [0 1 0 -1]) (map #(+ y %) [-1 0 1 0]))
    ; (for [tx (range (- x 1) (+ x 2))
    ;       ty (range (- y 1) (+ y 2))
          :when (and (>= tx 0) (>= ty 0) (<= tx width) (<= ty height)
                     (not= [x y] [tx ty])
                     (not= (gget grid tx ty) 1)
                     (not (contains? closed [tx ty])))]
      [tx ty])))

(defn path
  "When we hit our target node, we need to work backwards starting from
  the target node, go from each node to its parent until we reach the
  starting node. That is our path."
  [end parent closed]
  (reverse
   (loop [p [end parent]
          n (closed parent)]
     (if (nil? n)
       p
       (recur (conj p n) (closed n))))))

(defn search
  ""
  ([grid start end]
    (search grid start end cost))
  ([grid start end cost-fn]
    (let [[sx sy] start
          [ex ey] end
          open (priority-map-by
                 (fn [x y]
                   (if (= x y)
                     0
                     (let [[f1 _ h1] x
                           [f2 _ h2] y]
                       (if (= f1 f2)
                         (if (< h1 h2) -1 1)
                         (if (< f1 f2) -1 1)))))
                 start (cost-fn start start end))
          closed {}
          width (-> grid first count dec)
          height (-> grid count dec)]
      (when (and (not= (gget grid sx sy) 1)
                 (not= (gget grid ex ey) 1))
        (search grid width height open closed start end cost-fn))))
  ([grid width height open closed start end cost-fn]
     (if-let [[coord [_ _ _ parent]] (peek open)]
       (if-not (= coord end)
         (let [closed (assoc closed coord parent)
               edges (edges grid width height closed coord)
               open (reduce
                      (fn [open edge]
                        (if (not (contains? open edge))
                          (assoc open edge (conj (cost-fn edge start end) coord))
                          (let [[_ pg] (open edge)
                                [nf ng nh] (cost-fn edge start end)]
                            (if (< ng pg)
                              (assoc open edge (conj [nf ng nh] coord))
                              open))))
                      (pop open) edges)]
           (recur grid width height open closed start end cost-fn))
         (path end parent closed)))))
