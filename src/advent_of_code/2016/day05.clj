(ns advent-of-code.2016.day05
  "Fifth day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]
           [pandect.algo.md5 :refer [md5]]))

(defn one
  "Function to compute the password to the door based on the rules in the puzzle.
  This is just computationally intensive, and not hard at all."
  [seed]
  (->> (for [n (range)
            :let [raw (md5 (str seed n))]
            :when (.startsWith raw "00000")]
         (nth raw 5))
       (take 8)
       (apply str)))

(defn two
  "Function to compute the password for the second door where the hash has the
  position and the value in it. This is a little more complex, but that's due
  to the state and the non-sequential processing. But that's not too bad."
  [seed]
  (let [pcm (loop [n 0
                   code {}]
              (let [[nn h] (loop [n' n]
                             (let [raw (md5 (str seed n'))]
                               (if-not (.startsWith raw "00000")
                                 (recur (inc n'))
                                 [n' raw])))
                    pos (- (int (nth h 5)) (int \0))
                    pv (nth h 6)]
                (if (or (contains? code pos) (< 7 pos))
                  (recur (inc nn) code)
                  (let [nc (assoc code pos pv)]
                    (if (< (count nc) 8)
                      (recur (inc nn) nc)
                      nc)))))]
    (apply str (map #(get pcm %) (range 8)))))
