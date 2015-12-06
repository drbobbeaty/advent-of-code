(ns advent-of-code.day02)

(def puzzle
  "This is the input from the site for the package sizes. Each line is a
  string:

    WxLxH

  and we need to parse these into dimensions of feet, and then sort them
  smallest to largest to make all the calculations easier."
  (let [fix (fn [s] (->> (.split s "x" -1)
                         (map #(Integer/parseInt %))
                         (sort)))]
    (->> (.split (slurp "resources/input/day02.txt") "\n" -1)
         (filter not-empty)
         (map fix))))

(defn paper
  "Function to calculate the total paper to order based on the sequences of
  packages supplied as a tuple of measurements."
  [pkgs]
  (if (coll? pkgs)
    (let [sz (fn [[l w h]] (+ (* 3 l w) (* 2 w h) (* 2 l h)))]
      (loop [ord 0
             pl pkgs]
        (if-let [fp (first pl)]
          (recur (+ ord (sz fp)) (rest pl))
          ord)))))

(defn ribbon
  "Function to calculate the total paper to order based on the sequences of
  packages supplied as a tuple of measurements."
  [pkgs]
  (if (coll? pkgs)
    (let [sz (fn [[l w h]] (+ (* 2 l) (* 2 w) (* l w h)))]
      (loop [ord 0
             pl pkgs]
        (if-let [fp (first pl)]
          (recur (+ ord (sz fp)) (rest pl))
          ord)))))
