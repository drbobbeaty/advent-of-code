(ns advent-of-code.2019.day20
  "Twentieth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long]]
            [clojure.set :refer [union]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def atoz
  "A set of all the upper-case characters"
  (set (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defn mdist
  "Function to calculate the Manhattan distance between the two points, and
  assuming the second is the origin, unless specified."
  [[x y] & [org]]
  (let [dx (- x (or (first org) 0))
        dy (- y (or (second org) 0))]
    (+ (Math/abs dx) (Math/abs dy))))

(defn parse-map
  "Function to take the map data for the maze, and convert it into a standard
  map of coordinates and contents - as well as a map of warps: where one [x y]
  takes you another [x y] in one step. This has to go both ways, so the warp
  map is double-entry."
  [data]
  (cond
    (string? data)
      (loop [src data
             row 0
             col 0
             tic 0
             tiles (transient {})
             port (transient [])]
        (if-let [px (first src)]
          (cond
            (= \newline px)
              (recur (rest src) (if (pos? col) (inc row) row) 0 0 tiles port)
            (= \space px)
              (recur (rest src) row (if (pos? col) (inc col) col) (inc tic) tiles port)
            (#{\. \#} px)
              (recur (rest src) row (inc col) (inc tic) (assoc! tiles [col row] px) port)
            (atoz px)
              (recur (rest src) row (if (pos? col) (inc col) col) (inc tic) tiles (conj! port [px [tic row]])))
          (let [pts (persistent! tiles)
                pps (persistent! port)
                dss (map first pts)
                mx (+ (apply max (map first dss)) 4)
                my (apply max (map second dss))
                tks (loop [ps pps
                           glob (transient [])]
                      (if-let [[fc fp] (first ps)]
                        (let [[sc sp] (first (filter #(<= (mdist fp (second %)) 1) (rest ps)))
                              sk (if (< (mdist fp) (mdist sp)) (str fc sc) (str sc fc))
                              xb (max (first fp) (first sp))
                              yb (max (second fp) (second sp))
                              lpj (if (= (second fp) (second sp))
                                    (cond
                                      (= 0 yb)         [[(- xb 2) -1]  [(- xb 2) 0]]
                                      (<= my yb)       [[(- xb 2) (+ my 1)] [(- xb 2) my]]
                                      (< xb 2)         [[-1 yb]        [0 yb]]
                                      (<= xb (/ mx 2)) [[(- xb 3) yb]  [(- xb 4) yb]]
                                      (< (- mx 2) xb)  [[(- mx 3) yb]  [(- mx 4) yb]]
                                      :else            [[(- xb 2) yb]  [(- xb 1) yb]])
                                    (cond
                                      (< yb 2)         [[xb -1]             [xb 0]]
                                      (<= yb (/ my 2)) [[(- xb 2) (- yb 1)] [(- xb 2) (- yb 2)]]
                                      (< (- my 2) yb)  [[(- xb 2) my]       [(- xb 2) (- my 1)]]
                                      :else            [[(- xb 2) yb]       [(- xb 2) (+ yb 1)]]))]
                          (recur (remove #(= [sc sp] %) (rest ps))
                                 (conj! glob [(str fc sc) (if (#{"AA" "ZZ"} sk) (second lpj) lpj)])))
                        (persistent! glob)))
                warp (into {} (for [[k v] (group-by first tks)
                                    :let [trv (map second v)
                                          [[aj al] [bj bl]] trv]]
                                [k (if (and aj al bj bl) {aj bl, bj al} (first trv))]))]
            {:tiles pts :warp warp :tks tks})))
    (map? data)
      data
    (coll? data)
      (parse-map (cs/join "\n" data))))

(def puzzle
  "This is the input of the Pluto puzzle."
  (-> (slurp "resources/2019/input/day20.txt")
      (parse-map)))

(def trial1
  "Test data for the first part - it takes 23 steps"
  (parse-map ["         A           "
              "         A           "
              "  #######.#########  "
              "  #######.........#  "
              "  #######.#######.#  "
              "  #######.#######.#  "
              "  #######.#######.#  "
              "  #####  B    ###.#  "
              "BC...##  C    ###.#  "
              "  ##.##       ###.#  "
              "  ##...DE  F  ###.#  "
              "  #####    G  ###.#  "
              "  #########.#####.#  "
              "DE..#######...###.#  "
              "  #.#########.###.#  "
              "FG..#########.....#  "
              "  ###########.#####  "
              "             Z       "
              "             Z       "]))

(def trial2
  "Test data for the first part - it takes 58 steps"
  (parse-map ["                   A               "
              "                   A               "
              "  #################.#############  "
              "  #.#...#...................#.#.#  "
              "  #.#.#.###.###.###.#########.#.#  "
              "  #.#.#.......#...#.....#.#.#...#  "
              "  #.#########.###.#####.#.#.###.#  "
              "  #.............#.#.....#.......#  "
              "  ###.###########.###.#####.#.#.#  "
              "  #.....#        A   C    #.#.#.#  "
              "  #######        S   P    #####.#  "
              "  #.#...#                 #......VT"
              "  #.#.#.#                 #.#####  "
              "  #...#.#               YN....#.#  "
              "  #.###.#                 #####.#  "
              "DI....#.#                 #.....#  "
              "  #####.#                 #.###.#  "
              "ZZ......#               QG....#..AS"
              "  ###.###                 #######  "
              "JO..#.#.#                 #.....#  "
              "  #.#.#.#                 ###.#.#  "
              "  #...#..DI             BU....#..LF"
              "  #####.#                 #.#####  "
              "YN......#               VT..#....QG"
              "  #.###.#                 #.###.#  "
              "  #.#...#                 #.....#  "
              "  ###.###    J L     J    #.#.###  "
              "  #.....#    O F     P    #.#...#  "
              "  #.###.#####.#.#####.#####.###.#  "
              "  #...#.#.#...#.....#.....#.#...#  "
              "  #.#####.###.###.#.#.#########.#  "
              "  #...#.#.....#...#.#.#.#.....#.#  "
              "  #.###.#####.###.###.#.#.#######  "
              "  #.#.........#...#.............#  "
              "  #########.###.###.#############  "
              "           B   J   C               "
              "           U   P   P               "]))

(def trial3
  "Test data for the second part - it takes 396 steps"
  (parse-map ["             Z L X W       C                 "
              "             Z P Q B       K                 "
              "  ###########.#.#.#.#######.###############  "
              "  #...#.......#.#.......#.#.......#.#.#...#  "
              "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
              "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
              "  #.###.#######.###.###.#.###.###.#.#######  "
              "  #...#.......#.#...#...#.............#...#  "
              "  #.#########.#######.#.#######.#######.###  "
              "  #...#.#    F       R I       Z    #.#.#.#  "
              "  #.###.#    D       E C       H    #.#.#.#  "
              "  #.#...#                           #...#.#  "
              "  #.###.#                           #.###.#  "
              "  #.#....OA                       WB..#.#..ZH"
              "  #.###.#                           #.#.#.#  "
              "CJ......#                           #.....#  "
              "  #######                           #######  "
              "  #.#....CK                         #......IC"
              "  #.###.#                           #.###.#  "
              "  #.....#                           #...#.#  "
              "  ###.###                           #.#.#.#  "
              "XF....#.#                         RF..#.#.#  "
              "  #####.#                           #######  "
              "  #......CJ                       NM..#...#  "
              "  ###.#.#                           #.###.#  "
              "RE....#.#                           #......RF"
              "  ###.###        X   X       L      #.#.#.#  "
              "  #.....#        F   Q       P      #.#.#.#  "
              "  ###.###########.###.#######.#########.###  "
              "  #.....#...#.....#.......#...#.....#.#...#  "
              "  #####.#.###.#######.#######.###.###.#.#.#  "
              "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
              "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
              "  #.......#.....#.#...#...............#...#  "
              "  #############.#.#.###.###################  "
              "               A O F   N                     "
              "               A A D   M                     "]))

(defn bfs
  "Function to complete a breadth-first search (BFS) of the board with the
  set of wall pieces, and the warp map for jumping around. The target and
  starting positions are given as well. There is an optional parameter for
  using the recursive warp jumps in part 2 of the day's puzzle."
  [brd wall warp tgt pos & [rec]]
  (let [bts (keys brd)
        rmax (if (empty? bts) 0 (apply max (map second bts)))
        cmax (if (empty? bts) 0 (apply max (map first bts)))
        oob? (fn [[x y]] (or (neg? x) (< cmax x) (neg? y) (< rmax y)))
        updn (fn [p] (if (oob? p) -1 1))
        [px py] pos
        fin (concat tgt [0])]
    (loop [pts [[px py 0 0]]        ;; [x y lvl len]
           visit (set [[px py 0]])] ;; [x y lvl]
      (when-let [[x y lvl len] (first pts)]
        (if (= fin [x y lvl])
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

(defn one
  "Function to find the shortest distance from AA to ZZ using a simple BFS
  search through the maze. We simply want to find the shortest length, and
  don't care about the path this time."
  [& [arg]]
  (let [{brd :tiles wrp :warp :as src} (or arg puzzle)
        aa (get wrp "AA")
        zz (get wrp "ZZ")
        wm (apply merge (for [[k v] wrp :when (map? v)] v))]
    (bfs brd (set " #") wm zz aa)))

(defn two
  "Function to find the shortest distance from AA to ZZ using a simple BFS
  search through the maze - but this time each warp can take us up or down
  a level, so it's massively recursive. We simply want to find the shortest
  length, and don't care about the path this time."
  [& [arg]]
  (let [{brd :tiles wrp :warp :as src} (or arg puzzle)
        aa (get wrp "AA")
        zz (get wrp "ZZ")
        wm (apply merge (for [[k v] wrp :when (map? v)] v))]
    (bfs brd (set " #") wm zz aa true)))
