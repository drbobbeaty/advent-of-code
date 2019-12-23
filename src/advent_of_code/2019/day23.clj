(ns advent-of-code.2019.day23
  "Twenty-third day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum is-ascii?]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program - ASCII"
  (-> (slurp "resources/2019/input/day23.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn- network
  "Function to simulate a network of 50 Intcode machines all able to send
  and receive messages from one another. This will function in two ways - one
  for each part of the puzzle. For the first part, we'll stop on the first
  send to the NAT (addr 255). In the second, we'll allow the NAT to send data
  to the machine at id: 0, and then track all the Y values we're senging. When
  we get a double value, we can stop and show all the Y values."
  [mde]
  (let [ini (for [id (range 50)]
              {:id id :cpu (run {:memory puzzle :input [id] :io-wait true})})
        incoming (atom {})
        nat (atom [])
        natdy (atom [])
        dbl? (fn [s] (let [[a b] (take-last 2 s)] (and a b (= a b))))]
  (loop [net ini
         idle 0
         cnt 0]
    ;; check for everything being idle
    (when (and (= mde :part2) (< 150 idle) (every? empty? (vals @incoming)) (not-empty @nat))
      (infof "nat: %s" @nat)
      (swap! incoming update 0 concat @nat)
      (swap! natdy conj (second @nat))
      (reset! nat [])
    )
    ;; process the next machine in the network...
    (let [{id :id cpu :cpu} (first net)]
      (cond
        (dbl? @natdy)
          @natdy
        (= :io-in (:state cpu))
          (let [[x y] (take 2 (get @incoming id))
                ncpu (run (assoc cpu :input (concat (:input cpu) (if (and x y) [x y] [-1])) :io-wait true))]
            (if (and x y)
              (swap! incoming update id #(drop 2 %)))
            (recur (concat (rest net) [{:id id :cpu ncpu}]) (if (and x y) 0 (inc idle)) (inc cnt)))
        (= :io-out (:state cpu))
          (let [[a x y] (take 3 (:output cpu))]
            (if (and a x y)
              (if (= a 255)
                (case mde
                  :part1
                    (do
                      (infof "[%d] addr: %d : [%d %d]" id a x y)
                      {:id id :addr a :x x :y y})
                  :part2
                    (do
                      ; (infof "[%d] addr: %d : [%d %d]" id a x y)
                      (reset! nat [x y])
                      (recur (concat (rest net) [{:id id :cpu (update cpu :output #(drop 3 %))}]) 0 (inc cnt))))
                (do
                  (swap! incoming update a concat [x y])
                  (recur (concat (rest net) [{:id id :cpu (update cpu :output #(drop 3 %))}]) 0 (inc cnt))))
              (recur (concat (rest net) [{:id id :cpu (run (assoc cpu :io-wait true))}]) idle (inc cnt))))
        :else
          {:id id :cpu cpu})))))

(defn one
  "Function to imulate the 50 machines and stop on the first message send to
  the NAT at addr 255."
  []
  (network :part1))

(defn two
  "Function to use the NAT in it's indended mode, and to pass it's values to
  the machine with ID 0 - when things are idle. We will then keep track of the
  Y values sent from the NAT to machine 0, and when we have a double value in
  that list, we'll stop."
  []
  (network :part2))
