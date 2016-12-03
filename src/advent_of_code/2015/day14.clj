(ns advent-of-code.2015.day14
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the reindeer capabilities."
  (for [l (cs/split-lines (slurp "resources/2015/input/day14.txt"))
        :let [[_ rn sp dur rst] (re-matches #"(.*?) can fly (\d*?) km/s for (\d*?) seconds, but then must rest for (\d*?) seconds." l)
              sp' (Integer/parseInt sp)
              dur' (Integer/parseInt dur)
              rst' (Integer/parseInt rst)]]
    [rn sp' dur' rst']))

(defn go
  "Function to see how far a reindeer would go at the end of a period of time."
  [[nm spd dur rst] tot]
  (loop [x 0
         rt tot]
    (cond
      (<= rt 0) x
      (<= dur rt) (recur (+ x (* dur spd)) (- rt dur rst))
      :else (+ x (* spd rt)))))

(defn race
  "Function to find the winner of the classic race for distance."
  [tot rds]
  (let [dis (for [rd rds] [(first rd) (go rd tot)])
        gd (group-by second dis)
        mdis (apply max (keys gd))]
    [mdis (map first (get gd mdis))]))

(defn multi-race
  "Function to run the second race, tallying points every second for the
  leader."
  [tot rds]
  (frequencies (flatten (for [d (map inc (range tot))]
                          (second (race d rds))))))
