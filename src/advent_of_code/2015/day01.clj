(ns advent-of-code.2015.day01)

(def puzzle
  "This is the input from the site for the elevator moves for Santa."
  (slurp "resources/2015/input/day01.txt"))

(defn trip
  "Function to calculate the floor Santa will be on with the provided
  instructions."
  [updn]
  (if (string? updn)
    (loop [fl 0
           inst updn]
      (case (first inst)
        \( (recur (inc fl) (rest inst))
        \) (recur (dec fl) (rest inst))
        fl))))

(defn enter-basement
  "Function to return the position of the character in the instructions
  (1-biased) that causes Santa to enter the basement of the building
  (floor == -1)."
  [updn]
  (if (string? updn)
    (loop [fl 0
           inst updn
           step 1]
      (if-let [fl' (case (first inst)
                     \( (inc fl)
                     \) (dec fl)
                     nil (first inst)
                     fl)]
        (cond
          (neg? fl') step
          :else (recur fl' (rest inst) (inc step)))))))
