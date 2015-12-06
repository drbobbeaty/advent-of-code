(ns advent-of-code.day06)

(def board (repeat 1000 (repeat 1000 false)))

(def puzzle
  "This is the input from the site for the lighting plans from Santa."
  (let [pint (fn [s] (Integer/parseInt s))]
    (for [l (.split (slurp "resources/input/day06.txt") "\n" -1)
          :let [chg (cond
                      (.startsWith l "turn on") :on
                      (.startsWith l "turn off") :off
                      (.startsWith l "toggle") :toggle)
                [x1 y1 x2 y2] (rest (re-matches #".*?(\d+),(\d+).*?(\d+),(\d+)" l))]
          :when (and x1 y1 x2 y2)]
      [chg [(pint x1) (pint y1)] [(pint x2) (pint y2)]])))

(defn light
  "Function to find out the final state of any one light in the light board
  based on it starting off, and applying Santa's plan for the lights. This
  idea is the basic ray-tracing idea - follow one light through the process
  and see what it's eventual value is."
  [[x y] plan]
  (loop [lit false
         acts plan]
    (if-let [sa (first acts)]
      (let [[sc [x1 y1] [x2 y2]] sa
            lit' (if (and (<= x1 x x2) (<= y1 y y2))
                   (case sc
                     :on  true
                     :off false
                     :toggle (not lit))
                   lit)]
        (recur lit' (rest acts)))
      lit)))

(defn count-lights
  "Function to run through all the lights on the board, and count up the ones
  that are on after all the operations are done."
  [plan]
  (count (filter #(light % plan) (for [row (range 1000)
                                       col (range 1000)]
                                   [row col]))))

(defn dimmer
  "Function to follow the Ancient Elvish plans and assume the lights aren't
  just on or off, but they have a variable value that can be no less than 0,
  but can go very high."
  [[x y] plan]
  (loop [lit 0
         acts plan]
    (if-let [sa (first acts)]
      (let [[sc [x1 y1] [x2 y2]] sa
            lit' (if (and (<= x1 x x2) (<= y1 y y2))
                   (case sc
                     :on  (inc lit)
                     :off (max 0 (dec lit))
                     :toggle (+ 2 lit))
                   lit)]
        (recur lit' (rest acts)))
      lit)))

(defn count-lumens
  "Function to run through all the lights on the board, and this time, count
  up the lumens of the board - one light at a time."
  [plan]
  (reduce + (map #(dimmer % plan) (for [row (range 1000)
                                       col (range 1000)]
                                   [row col]))))
