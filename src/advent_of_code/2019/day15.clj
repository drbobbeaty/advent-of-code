(ns advent-of-code.2019.day15
  "Fifteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program for the repair droid."
  (-> (slurp "resources/2019/input/day15.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn render
  "Function to render the display of the board, as defined in the puzzle for
  walls, and the oxygen system. We added a code for the starting origin to
  make it a little easier to see."
  [tiles]
  (let [bts (keys @tiles)
        rvs (map second bts)
        [minr maxr] (if (empty? rvs) [0 0] [(apply min rvs) (apply max rvs)])
        cvs (map first bts)
        [minc maxc] (if (empty? cvs) [0 0] [(apply min cvs) (apply max cvs)])
        rows (inc (- maxr minr))
        cols (inc (- maxc minc))]
    (concat [{:oxygen (for [[p c] @tiles :when (= 2 c)] p)}]
      (for [r (range rows)]
        (->> (for [c (range cols) :let [p [(+ c minc) (+ r minr)]]]
               (case (@tiles p)
                 0 "+"
                 2 "O"
                 3 "X"
                 " "))
             (apply str))))))

(defn explore
  "Function to explore in all directions, recursively, to find out the entire
  extent of the system and then report back with what that structure is."
  [cpu brd [x y]]
  (let [mov (for [[p inp] [[[x (inc y)] 1] [[x (dec y)] 2] [[(dec x) y] 3] [[(inc x) y] 4]]
                  :when (not (contains? @brd p))
                  :let [ncpu (run (assoc cpu :input [inp] :output [] :io-wait true))]]
              {:cpu ncpu :pos p :out (first (:output ncpu))})]
    ; (infof "exploring: [%d %d]" x y)
    ;; update all the points we have found data on
    (doseq [mi mov] (swap! brd assoc (:pos mi) (:out mi)))
    ;; now explore the points that were not walls
    (doseq [mi mov]
      (if (= 1 (:out mi))
        (explore (:cpu mi) brd (:pos mi))))))

(defn one
  "Function to map out the compelte space that the robot can go. And then
  render it in a way that I can just figure out what the answer is. The
  output of this is:

          +++++ +++++++++++ +++++++++++++++++++++
         +     +...........+.................    +
         + +++++.+++++++++.+.+++++++++++++++.++++
         +.....+.+.......+.+...+...........+.+...+
         +.+++.+.+.+++++.+.+++.+.+++++++++.+.+.+.+
         +...+.+.+.+...+...+...+...+   +...+.+.+.+
          ++.+.+.+.+.+.+++++.+++++.+ +++.+++.+.+.+
          O+.+...+...+.......+.....+    .+  ...+.+
         +.+.+++++++++++++++++.+++++++++.+++++++.+
         +.+.+     +.........+.........+.........+
         +.+.+++++ +.+++++++.+++++ +++.+++++++++ +
         +.+...+   +.+     +.....+   +...+.....+ +
         +.+++.+ +++.+++++ +++++.+++++ +.+.+++.+ +
         +.....  +...+...+     +.....+ +.+.+...+ +
          ++++++ +.+++.+.+++ +++++++.+ +.+.+.++++
         +...+   +.....+.+   +.......+ +...+.....+
         +.+.+++++++++++.+ +++.+++++++ +++++++++.+
         +.+.......  +...+   +.+     + +.........+
         +.+++++++.+++.+++++ +.+ + + + +.+++++++ +
         +.+     +.....+...+ +X+ + + + +...+   + +
         +.+++++ +++++++.+.+ +++ + +++ +++.+ + + +
         +.+.............+...+   +     +...+ +   +
         +.+.+++++++++++++++.+++++ +++++.+++ ++++
         +...+   +   +     +.+...+ +...+.+ +   + +
          ++++ + + + + + + +.+.+.+++.+.+.+ +++ + +
         + +   +   +   + + +.+.+.....+.+.+     + +
         + + +++++++++++ + +.+.+++++++.+.+++ +++ +
         + + +       +   + +.+.+     +.+...+     +
         + + + +++ +++ +++ +.+.+++++ +.+++.+++++ +
         +   +   + +   +   +...+     +...+...+   +
         + +++++++ + +++++++++++ +++ +++.+++.+ ++
         + +     + +             +     +.....+   +
         + + +++ + +++++++++++ ++++++++ +++++ ++ +
         +   + + +           +     +   +     +   +
          ++++ + +++++++ +++ +++++ + + + +++ + ++
         +     +   +   +   +   + +   +   + +   + +
         + ++++ ++ + + +++++ + + +++++++++ +++++ +
         +     +   + + +   + +     +     +     + +
         + +++ + +++ + + + + +++++ +++ + + +++ + +
         +   +       +   +   +         +   +     +
          +++ +++++++ +++ +++ +++++++++ +++ +++++

  and then I put dots in the path to get from the X (origin) to the O
  and then used the command:

      $ tr -cd '.' < junk.txt | wc -c

  to get the number of dots, and added one for the step to the O. This
  was the right answer."
  [& [mem]]
  (let [cpu (if (map? mem) mem {:memory (or mem puzzle)})
        brd (atom {[0 0] 3})]
    ;; explore the board
    (explore cpu brd [0 0])
    ;; display the board
    (render brd)))

(defn fill-up
  "Function to look at the board, find all the spots with oxygen in them,
  and then for all points N, E, S, W of that point, see if oxygen can go
  there as well. If so, update the board, and count the hits. This will make
  a simple diffusion map of the space as it fills up with oxygen."
  [brd]
  (let [hits (atom 0)]
    (doseq [[[x y] c] @brd
            :when (= 2 c)
            p [[x (inc y)] [x (dec y)] [(dec x) y] [(inc x) y]]]
      (when (= 1 (@brd p))
        (swap! hits inc)
        (swap! brd assoc p 2)))
    @hits))

(defn two
  "Function to count how many minutes it takes for oxygen to fill up the
  space in the ship. This is spreading uniformly, and each call to 'fill-up'
  will take a minute, and diffuse to any un-oxygenated spaces. It's not a
  bad little trick."
  [& [mem]]
  (let [cpu (if (map? mem) mem {:memory (or mem puzzle)})
        brd (atom {[0 0] 1})]
    ;; explore the board
    (explore cpu brd [0 0])
    ;; loop to count how many minutes it takes to fill the space
    (loop [cnt 0]
      (let [hits (fill-up brd)]
        ; (infof "cnt: %d hits: %d" cnt hits)
        ; (doseq [l (render brd)] (info l))
        (if (pos? hits)
          (recur (inc cnt))
          cnt)))))
