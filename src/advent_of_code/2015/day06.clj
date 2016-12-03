(ns advent-of-code.2015.day06)

(def board (repeat 1000 (repeat 1000 false)))

(def puzzle
  "This is the input from the site for the lighting plans from Santa."
  (let [pint (fn [s] (Integer/parseInt s))]
    (for [l (.split (slurp "resources/2015/input/day06.txt") "\n" -1)
          :let [chg (cond
                      (.startsWith l "turn on") :on
                      (.startsWith l "turn off") :off
                      (.startsWith l "toggle") :toggle)
                [x1 y1 x2 y2] (rest (re-matches #".*?(\d+),(\d+).*?(\d+),(\d+)" l))]
          :when (and x1 y1 x2 y2)]
      [chg [(pint x1) (pint y1)] [(pint x2) (pint y2)]])))

(defn elves
  "These are the rules the elves started with."
  [sc lit]
  (case sc
    :on  1
    :off 0
    :toggle (mod (inc lit) 2)))

(defn santa
  "These are the rules that Santa corrected."
  [sc lit]
  (case sc
    :on  (inc lit)
    :off (max 0 (dec lit))
    :toggle (+ 2 lit)))

(defn light
  "Function to follow the Ancient Elvish plans and assume the lights aren't
  just on or off, but they have a variable value that can be no less than 0,
  but can go very high. The `rule` tells us what the next state of the light
  is given the current state, and the transition from the directions."
  [[x y] plan rule]
  (loop [lit 0
         acts plan]
    (if-let [sa (first acts)]
      (let [[sc [x1 y1] [x2 y2]] sa
            lit' (if (and (<= x1 x x2) (<= y1 y y2))
                   (rule sc lit)
                   lit)]
        (recur lit' (rest acts)))
      lit)))

(defn count-lumens
  "Function to run through all the lights on the board, and this time, count
  up the lumens of the board - one light at a time. The `rule` is either the
  `elves` or `santa` for part 1 or 2 of the advent challenge."
  [plan rule]
  (reduce + (pmap #(light % plan rule) (for [row (range 1000)
                                             col (range 1000)]
                                         [row col]))))
