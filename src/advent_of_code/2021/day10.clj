(ns advent-of-code.2021.day10
  "Tenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the navigation subsystem syntax from the sub."
  (-> (slurp "resources/2021/input/day10.txt")
    (trim)
    (split #"\n")
    (->> (map seq))))

(def test1
  "Test data for the first part."
  (-> ["[({(<(())[]>[[{[]{<()<>>"
       "[(()[<>])]({[<{<<[]>>("
       "{([(<{}[<>[]}>{[]{[(<()>"
       "(((({<>}<{<{<>}{[]{[]{}"
       "[[<[([]))<([[{}[[()]]]"
       "[{[{({}]{}}([{[{{{}}([]"
       "{<[[]]>}<{[{[{[]{()[[[]"
       "[<(<(<(<{}))><([]([]()"
       "<{([([[(<>()){}]>(<<{{"
       "<{([{{}}[<[[[<>{}]]]>[]]"]
    (->> (map seq))))

(defn opener?
  "Predicate function to simply indicate of the provided character is an
  'opener' of a pair. This will be necessary as we then need to push it on
  the stack and continue processing. It's when we pop them off that things
  need to be checked."
  [c]
  (<= 0 (.indexOf (seq "({[<") c)))

(defn mate
  "Simple function to find the 'closing' mate for an 'opener' - this is the
  way we know what to *expect* on a non-opener, based on the top of the
  stack of openers we have already encountered."
  [c]
  (case c
   \( \)
   \[ \]
   \{ \}
   \< \>))

(defn bads
  "Simple look-up to return the number of points for an unexpected character,
  a 'bad' character, in the syntax file."
  [c]
  (case c
   \) 3
   \] 57
   \} 1197
   \> 25137))

(defn missing
  "Function to score the missing closing blocks with the scheme presented
  in the puzzle. This requires a little more complex scoring system as
  we don't look at the first failing character, but it's not bad, and we
  return the score just the same for inclusion in the output."
  [coll]
  (let [pts (fn [c] (.indexOf (seq " )]}>") c))]
    (loop [cls (map mate coll)
           ret 0]
      (if-let [c (first cls)]
        (recur (rest cls) (+ (* ret 5) (pts c)))
        ret))))

(defn check
  "Function to take a line of syntax as a sequence of characters, and a
  stack of the active opens, in LIFO order, and see if there is a
  syntax error in the line. This could end up with unclosed blocks,
  or it could be an error - either way, this will process it to the end."
  [coll ops]
  (if-let [c (first coll)]
    (if (opener? c)
      (recur (rest coll) (concat [c] ops))
      (let [top (first ops)]
        (if (= (mate top) c)
          (recur (rest coll) (rest ops))
          {:expected (mate top) :found c :points (bads c)})))
    {:left ops :points (missing ops)}))

(defn one
  "Function to find the errors in each line, scored with the points for
  that kind of error, and then sum the points. It's a simple call to
  check, and then to sum those points."
  [& [coll]]
  (->> (for [l puzzle] (check l []))
    (filter :expected)
    (map :points)
    (sum)))

(defn two
  "Function to find the median score of all the *incomplete* syntax
  lines, also scored by the check function. It's just a different filter
  on the output, and a different aggregation function."
  [& [coll]]
  (->> (for [l puzzle] (check l []))
    (filter :left)
    (map :points)
    (median)))
