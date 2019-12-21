(ns advent-of-code.2019.day20
  "Twentieth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long ucase]]
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

(defn encode
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
                          (recur (remove #(or (= [fc fp] %) (= [sc sp] %)) ps)
                                 (conj! glob [(str fc sc) (if (= fc sc) (second lpj) lpj)])))
                        (persistent! glob)))
                warp (into {} (for [[k v] (group-by first tks)
                                    :let [trv (map second v)
                                          [[aj al] [bj bl]] trv]]
                                [k (if (and aj al bj bl) {aj bl, bj al} (first trv))]))]
            {:tiles pts :warp warp :tks tks})))
    (map? data)
      data
    (coll? data)
      (encode (cs/join "\n" data))))

(defn render
  "Function to render the map as it exists in the argument. The keys are
  [x y] (col, row) - and the values are the contents to display."
  [{tiles :tiles warp :warp :as arg}]
  (let [bts (keys tiles)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))
        cols (if (empty? bts) 0 (inc (apply max (map first bts))))]
    (concat
    (for [r (range rows)]
      (apply str (for [c (range cols)] (get tiles [c r] " "))))
    warp
    )))

(def puzzle
  "This is the input of the Pluto puzzle."
  (-> (slurp "resources/2019/input/day20.txt")
      (encode)))

(def trial1
  "Test data for the first part - it takes 23 steps"
  (encode ["         A           "
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
  (encode ["                   A               "
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

(defn explore
  "Function to explore the surrounding squares of [x y] and see if we can
  move into them - and if so, see where they lead. This is going to be
  called recursively to walk every possible path until we get to one of
  the targets (tgt), and then we'll stop - indicating the path we took to
  get here, and what it is we hit."
  [brd wall warp tgt stps [x y]]
  (let [bts (keys brd)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))
        cols (if (empty? bts) 0 (inc (apply max (map first bts))))
        oob? (fn [[x y]] (or (neg? x) (<= cols x) (neg? y) (<= rows y)))
        mov (for [p [[x (inc y)] [x (dec y)] [(dec x) y] [(inc x) y]]
                  :let [p' (get warp p p)]
                  :when (not (or (wall (brd p')) (oob? p') (stps p')))
                  :let [nstps (assoc stps p' (count stps))]]
              {:stps nstps :pos p'})
        ans (atom [])]
    (doseq [mi mov]
      (if (= tgt (:pos mi))
        (swap! ans conj {:target tgt :path (assoc (:stps mi) (:pos mi) (count (:stps mi)))})
        (swap! ans concat (explore brd wall warp tgt (:stps mi) (:pos mi)))))
    @ans))

(defn one
  ""
  [& [arg]]
  (let [{src :tiles wrp :warp} (or arg puzzle)
        spos (get wrp "AA")
        blk (set " #")
        port (apply merge (filter map? (vals wrp)))]
    (->> (explore src blk port (get wrp "ZZ") {spos 0} spos)
         (map #(dec (count (:path %))))
         (sort)
         (first))
  ))
