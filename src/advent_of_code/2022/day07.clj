(ns advent-of-code.2022.day07
  "Seventh day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the terminal log from looking at the system at hand."
  (-> (slurp "resources/2022/input/day07.txt")
    (trim)
    (split #"\n")))

(def test1
  "Test data for the first part."
  (-> ["$ cd /"
       "$ ls"
       "dir a"
       "14848514 b.txt"
       "8504156 c.dat"
       "dir d"
       "$ cd a"
       "$ ls"
       "dir e"
       "29116 f"
       "2557 g"
       "62596 h.lst"
       "$ cd e"
       "$ ls"
       "584 i"
       "$ cd .."
       "$ cd .."
       "$ cd d"
       "$ ls"
       "4060174 j"
       "8033020 d.log"
       "5626152 d.ext"
       "7214296 k"]))

(defn map-dirs
  "Function to take the terminal log and map out the size of all the
  directories on the drive, so that it can be manipulated as needed."
  [coll]
  (let [dirs (atom {})
        cwd (atom [])]
    (loop [cmds coll]
      (if-let [cmd (first cmds)]
        (cond
          (.startsWith cmd "$ cd")
            (let [[_ dir] (re-matches #"\$ cd (.+)" cmd)
                  k (keyword dir)]
              (cond
                (= k :/)  (reset! cwd [:/])
                (= k :..) (swap! cwd butlast)
                :else     (swap! cwd concat [k]))
              (let [cd (cs/join ":" (map name @cwd))]
                (if-not (contains? @dirs cd)
                  (swap! dirs assoc cd 0)))
              (recur (rest cmds)))
          (.startsWith cmd "$ ls")
            (recur (rest cmds))
          (.startsWith cmd "dir")
            (recur (rest cmds))
          :else
            (let [[_ raw fname] (re-matches #"(\d+) (.+)" cmd)
                  sz (parse-int raw)]
              (doseq [i (range (count @cwd))
                      :let [cd (cs/join ":" (map name (take (inc i) @cwd)))]]
                (swap! dirs assoc cd (+ sz (get @dirs cd))))
              (recur (rest cmds))))))
    @dirs))

(defn one
  "Function to find the sum of all directories with a size of less than
  100,000 bytes - even if it's double-counting the files."
  [& [coll]]
  (let [dirs (map-dirs puzzle)]
    (sum (filter #(<= % 100000) (vals dirs)))))

(defn two
  "Function to find the smallest directory to delete to create the necessary
  free space to do the upgrade of the system."
  [& [coll]]
  (let [dirs (map-dirs puzzle)
        free (- 70000000 (get dirs "/"))
        need (- 30000000 free)]
    (first (filter #(<= need %) (sort (vals dirs))))))
