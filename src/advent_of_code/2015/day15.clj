(ns advent-of-code.2015.day15
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the cookie ingredients."
  (into {} (for [l (cs/split-lines (slurp "resources/2015/input/day15.txt"))
                 :let [parts (.split (cs/replace l #":|," "") " ")
                       prop (into {} (for [[k v] (partition 2 (rest parts))]
                                       [(keyword k) (Integer/parseInt v)]))]]
    [(first parts) prop])))

(defn score
  "Function to calculate the 'score' of a cookie based on a recipe that is a
  map of ingredient/quantity pairs, and a list of ingredient properites like
  the `puzzle`. This score will not include the 'calories' property - if it
  exists."
  [rec ing & [ccnt]]
  (let [sc (for [[i qty] rec]
             (into {} (for [[k v] (get ing i)] [k (* v qty)])))
        tot (apply * (for [k (remove #(= :calories %) (keys (first sc)))]
                       (max 0 (apply + (map k sc)))))
        cals (apply + (map :calories sc))]
    (cond
      ccnt (if (= ccnt cals) tot 0)
      :else tot)))

(defn breakdown
  "Function to return a sequence of the different ways you can sum `cnt`
  integers to equal `tot`. This is how we will know the possible ways of
  creating a recipe."
  [cnt tot]
  (if (= 1 cnt)
    [[tot]]
    (apply concat (for [i (map inc (range (- tot (dec cnt))))
                        :let [more (breakdown (dec cnt) (- tot i))]]
                    (map #(flatten (concat [i] %)) more)))))

(defn best-cookie
  "Function to compute the best cookie score based on Santa's rules and
  given the puzzle components and the total volume of the batch for each
  cookie run."
  [pz & [vol ccnt]]
  (let [il (keys pz)]
    (apply max (for [mr (breakdown (count il) (or vol 100))
                     :let [cr (into {} (map vector il mr))]]
                 (score cr pz ccnt)))))
