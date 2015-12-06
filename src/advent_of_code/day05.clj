(ns advent-of-code.day05)

(def puzzle
  "This is the input from the site for the elevator moves for Santa."
  (.split (slurp "resources/input/day05.txt") "\n" -1))

(def vowels #{\a \e \i \o \u})

(defn nice?
  "Function to see if the provided string is `nice` based on Santa's rules."
  [s]
  (if (string? s)
    (and (some #(= 1 %) (map #(count (distinct %)) (partition 2 1 s)))
         (<= 3 (count (filter vowels s)))
         (neg? (.indexOf s "ab"))
         (neg? (.indexOf s "cd"))
         (neg? (.indexOf s "pq"))
         (neg? (.indexOf s "xy")))))

(defn nice-cnt
  "Function to count the number of `nice?` strings in the provided sequence."
  [lst]
  (count (filter nice? lst)))

(defn doubles?
  "Predicate function to check the 'pair of doubles' rule for the `nicer?`
  function."
  [arg]
  (if (string? arg)
    (loop [s arg]
      (if (< 3 (count s))
        (let [ft (apply str (take 2 s))
              rs (apply str (drop 2 s))]
          (if (<= 0 (.indexOf rs ft))
            true
            (recur (rest s))))
        false))))

(defn nicer?
  "Predicate function to see if Santa's second set of rules for nice(er) words
  is true for the provided string."
  [s]
  (if (string? s)
    (and (doubles? s)
         (some #(= (first %) (last %)) (partition 3 1 s)))))

(defn nicer-cnt
  "Function to count the number of `nicer?` strings in the provided sequence."
  [lst]
  (count (filter nicer? lst)))
