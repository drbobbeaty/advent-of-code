(ns advent-of-code.2016.day11
  "Eleventh day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(defn code-it
  "Function to do the simple parsing of the input from the AoC site. Just
  makes sense to provide some structure to the data so that it's easier to
  work with."
  [s]
  (let [pint (fn [x] (Integer/parseInt (cs/trim x)))
        puts (atom [])
        moves (atom [])]
    (cond
      (string? s) (cond
                    (.startsWith s "bot")
                      (let [[sb lt ln ht hn] (drop 1 (re-matches #"^bot (\d+) gives low to (.+?) (\d+) and high to (.+?) (\d+)$" s))]
                        {:source (keyword (str "bot-" (pint sb)))
                         :low (keyword (str lt "-" (pint ln)))
                         :high (keyword (str ht "-" (pint hn)))})
                    (.startsWith s "value")
                      (let [[v b] (drop 1 (re-matches #"^value (\d+) goes to bot (\d+)$" s))]
                        {:value (pint v)
                         :dest (keyword (str "bot-" (pint b)))}))
      (coll? s)   (map code-it s)
      :else       s)))

(def puzzle
  "This is the instructions for the bot control for the chip movement."
  (-> (slurp "resources/2016/input/day10.txt")
      (cs/trim)
      (cs/split #"\n")
      (code-it)))

(defn run
  "Function to run all the instructions in the source string - as dictated by
  the instructions, and updating the state of the world as needed. This will
  be a multi-pass system, looping over the undone instructions until they are
  all done, and then returning the state of the world."
  [& [src]]
  (let [world (atom {})
        raw (or (code-it src) puzzle)
        hit (fn [m] (cond
                      (:value m)  (do
                                    (swap! world update (:dest m) conj (:value m))
                                    nil)
                      (:source m) (let [bd ((:source m) @world)]
                                    (if (= 2 (count bd))
                                      (let [l (apply min bd)
                                            h (apply max bd)]
                                        (if (= [l h] [17 61])
                                          (prn m bd))
                                        (swap! world dissoc (:source m))
                                        (swap! world update (:low m) conj l)
                                        (swap! world update (:high m) conj h)
                                        nil)
                                      m))))]
    (loop [inst (or (code-it src) puzzle)]
      (let [cyc (remove nil? (map hit inst))]
        (if (pos? (count cyc))
          (recur cyc))))
    @world))

(defn one
  "Function to take a sequence of instructions as the argument, or use the
  puzzle as the default, and return the state of the world once it's all
  done."
  [& [src]]
  (run src))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src ["value 5 goes to bot 2"
             "bot 2 gives low to bot 1 and high to bot 0"
             "value 3 goes to bot 1"
             "bot 1 gives low to output 1 and high to bot 0"
             "bot 0 gives low to output 2 and high to output 0"
             "value 2 goes to bot 2"]]
    (run src)))
