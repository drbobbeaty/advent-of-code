(ns advent-of-code.2022.day14
  "Fourteenth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to parse the description of the rock walls into points that
  I can place in a map, to represent the board as it evolves."
  [s]
  (let [pts (->> (split s " -> ")
              (map #(split % ","))
              (map #(map parse-int %)))]
    (loop [[x y] (first pts)
           src (rest pts)
           dots []]
      (if-let [[ex ey] (first src)]
        (let [lx (min x ex)
              ly (min y ey)
              hx (max x ex)
              hy (max y ey)
              ndots (if (= x ex)
                      (map vector (repeat x) (range ly (inc hy)))
                      (map vector (range lx (inc hx)) (repeat y)))]
          (recur [ex ey] (rest src) (concat dots ndots)))
        dots))))

(def puzzle
  "This is the input of where the rock walls are in the cave."
  (-> (slurp "resources/2022/input/day14.txt")
    (trim)
    (split #"\n")
    (->> (map parse)
      (apply concat))
    (as-> pts (map vector pts (repeat \#)))
    (->> (into {}))))

(def test1
  "Test data for the first part."
  (-> ["498,4 -> 498,6 -> 496,6"
       "503,4 -> 502,4 -> 502,9 -> 494,9"]
    (->> (map parse)
      (apply concat))
    (as-> pts (map vector pts (repeat \#)))
    (->> (into {}))))

(defn fall
  "Function to simulate the falling of one bit of sand. This will fall
  straight down until it hits something, and then stop- kinda, and it is
  meant to either have a floor to the cave, or not, and if not, this
  might return nil as there is no place the sand comes to rest."
  ([brd p] (fall brd p nil))
  ([brd p yfloor]
    (let [ymax (apply max (map second (keys brd)))]
      (loop [[x y] p]
        (if (or yfloor (<= y ymax))
          (let [nd [x (inc y)]
                nl [(dec x) (inc y)]
                nr [(inc x) (inc y)]]
            (cond
              (= yfloor (inc y))     [x y]
              (= \. (get brd nd \.)) (recur nd)
              (= \. (get brd nl \.)) (recur nl)
              (= \. (get brd nr \.)) (recur nr)
              :else                  [x y])))))))

(defn show
  "Display the board like they do in AoC - just to see what's up."
  [brd]
  (let [xseq (map first (keys brd))
        yseq (map second (keys brd))
        xl (apply min xseq)
        xh (apply max xseq)
        yl (apply min yseq)
        yh (apply max yseq)]
    (for [y (range yl (inc yh))]
      (apply str
        (for [x (range xl (inc xh))]
          (get brd [x y] \.))))))

(defn one
  "Function to find the number of sand units that can fall and stay in place
  without falling into the abyss."
  [& [coll]]
  (loop [brd puzzle
         cnt 0]
    (if-let [np (fall brd [500 0])]
      (recur (assoc brd np \o) (inc cnt))
      {:count cnt})))

(defn two
  "Function to find the number of sand units that have to drop to make it
  so that the dropped sand stays at the start of the drop. This is only
  possible with a floor that the sand has to eventually come to rest on."
  [& [coll]]
  (let [init puzzle
        yfloor (+ 2 (apply max (map second (keys init))))]
    (loop [brd init
           cnt 0]
      (let [np (fall brd [500 0] yfloor)]
        (if (= np [500 0])
          {:count (inc cnt)}
          (recur (assoc brd np \o) (inc cnt)))))))
