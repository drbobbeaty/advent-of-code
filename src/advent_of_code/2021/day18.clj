(ns advent-of-code.2021.day18
  "Eighteenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim] :as util]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the hex byte stream of the packet from the elves on the
  ship."
  (-> (slurp "resources/2021/input/day18.txt")
    (trim)
    (util/split #"\n")))

(def test1
  "Test data for the first part. Ans: [[[[1,1],[2,2]],[3,3]],[4,4]]"
  ["[1,1]"
   "[2,2]"
   "[3,3]"
   "[4,4]"])

(def test2
  "Test data for the first part. Ans: [[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
  ["[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"])

(def test3
  "Test data for the first part. Ans: [[[[3,0],[5,3]],[4,4]],[5,5]]"
  ["[1,1]"
   "[2,2]"
   "[3,3]"
   "[4,4]"
   "[5,5]"])

(def test4
  "Test data for the first part. Ans: [[[[5,0],[7,4]],[5,5]],[6,6]]"
  ["[1,1]"
   "[2,2]"
   "[3,3]"
   "[4,4]"
   "[5,5]"
   "[6,6]"])

(def test5
  "Test data for the first part. Ans: [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
  ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
   "[[[5,[2,8]],4],[5,[[9,9],0]]]"
   "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
   "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
   "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
   "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
   "[[[[5,4],[7,7]],8],[[8,3],8]]"
   "[[9,3],[[9,9],[6,[4,9]]]]"
   "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
   "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"])

(def test6
  "Test data for the second part."
  ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
   "[[[5,[2,8]],4],[5,[[9,9],0]]]"
   "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
   "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
   "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
   "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
   "[[[[5,4],[7,7]],8],[[8,3],8]]"
   "[[9,3],[[9,9],[6,[4,9]]]]"
   "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
   "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"])

(defn find-explode
  "Function to find if an explode needs to be done, and if so, return a
  sequence like:

    ['prefix' 5 4 'suffix']

  and if no explode needs to be done, then:

    ['string' nil nil nil]
  "
  [s]
  (loop [src s
         dpth 0
         pre []]
    (if-let [c (first src)]
      (let [ndpth (case c
                    \[ (inc dpth)
                    \] (dec dpth)
                    dpth)]
        (if (= 5 ndpth)
          (let [[_ a b r] (re-matches #"^(\d+),(\d+)\](.*)$" (apply str (rest src)))]
            [(apply str pre) (parse-int a) (parse-int b) r])
          (recur (rest src) ndpth (conj pre c))))
      [(apply str pre) nil nil nil])))

(defn explode
  "Function to see if there needs to be an 'explode' operation on the snailfish
  number provided, and if so, then to do it, and return. It only does this once
  as those are the rules - for the left-most nested pair."
  [s]
  (let [[pre a b post] (find-explode s)]
    (if (= nil a b post)
      s
      (let [[_ lbeg lno lend] (re-matches #"^(.*[\[\,\]])(\d+)(.+?)$" pre)
            [_ rbeg rno rend] (re-matches #"^(.+?)(\d+)([\[,\]].*)$" post)]
        (->> [(if lno [lbeg (+ (parse-int lno) a) lend] pre)
              [0]
              (if rno [rbeg (+ (parse-int rno) b) rend] post)]
          (apply concat)
          (apply str))))))

(defn split
  "Function to see if a snailfish number needs to be split, and if so, then to
  do it and return the result. Again, this does this only once, on the left-
  most number that fits the criteria, and then returns."
  [s]
  (loop [src s
         pre ""]
    (let [[_ lft no rgt] (re-matches #"^(.+?)(\d+)(.*)$" src)]
      (if (= nil lft no rgt)
        (apply str pre src)
        (let [n (parse-int no)
              lo (quot n 2)
              hi (- n lo)]
          (if (<= 10 n)
            (str pre lft "[" lo "," hi "]" rgt)
            (recur rgt (str pre lft no))))))))

(defn add
  "Function to add up two snailfish numbers, and then reduce them, as needed,
  based on the rules, and return the result."
  [a b]
  (let [red (fn [s]
              (let [ex (explode s)]
                (if (not= ex s)
                  ex
                  (let [sp (split s)]
                    (if (not= sp s) sp s)))))]
    (loop [sum (str "[" a "," b "]")]
      (let [nxt (red sum)]
        (if (not= nxt sum)
          (recur nxt)
          sum)))))

(defn mag
  "Function to compute the magnitude of a snailfish number and return it as
  an integer."
  [s]
  (loop [src s]
    (let [[_ f a b r] (re-matches #"^(.*)\[(\d+)\,(\d+)\](.*)$" src)]
      (if (= nil f a b r)
        (parse-int src)
        (recur (str f (+ (* 3 (parse-int a)) (* 2 (parse-int b))) r))))))

(defn one
  "Function to find the snailfish sum of all the numbers in the puzzle input
  and then also compute the magnitude, and return that."
  [& [coll]]
  (let [src puzzle]
    (loop [vals (rest src)
           sum (first src)]
      (if-let [v (first vals)]
        (recur (rest vals) (add sum v))
        {:sum sum :mag (mag sum)}))))

(defn two
  "Function to find the maximum magnitude of any two snailfish sums in the
  puzzle input. This is just a simple exhaustive search."
  [& [coll]]
  (let [n (count puzzle)]
    (->> (for [i (range n)
               j (range (inc i) n)
               :let [a (nth puzzle i)
                     b (nth puzzle j)
                     apb (mag (add a b))
                     bpa (mag (add b a))]]
           {:i i :j j :mag (max apb bpa)})
      (sort-by :mag)
      (last))))
