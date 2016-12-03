(ns advent-of-code.2015.day03)

(def puzzle
  "This is the input from the site for the directions to homes in his route."
  (slurp "resources/2015/input/day03.txt"))

(defn houses
  "Function to convert a sequence of directions (^v<>) in a string into the
  locations of homes he actually visits - in the order he visits them."
  [dirs]
  (if (string? dirs)
    (loop [loc [0 0]
           addrs (transient [[0 0]])
           rte dirs]
      (if-let [mv (first rte)]
        (let [[x y] loc
              loc' (case mv
                     \^ [x (inc y)]
                     \v [x (dec y)]
                     \> [(inc x) y]
                     \< [(dec x) y]
                     loc)]
          (recur loc' (conj! addrs loc') (rest rte)))
        (persistent! addrs)))))

(defn house-cnt
  "Function to get the count of distinct houses that Sants is visiting given
  the set of directions from the elves."
  [dirs]
  (count (distinct (houses dirs))))

(defn two-santas
  "Function to take the directions, split them into alternating paths and then
  let Santa take one, and Robo-Sants take the other and see how many unique
  homes are visited."
  [dirs]
  (if (string? dirs)
    (let [sp (houses (apply str (take-nth 2 dirs)))
          rsp (houses (apply str (take-nth 2 (rest dirs))))]
      (count (distinct (concat sp rsp))))))
