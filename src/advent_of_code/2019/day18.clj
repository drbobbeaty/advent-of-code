(ns advent-of-code.2019.day18
  "Eighteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long ucase median]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn encode
  "Function to take the map data from the initial discovery, and convert it
  into a more useful map of coordinates and contents. This makes it much
  easier to work with the data."
  [data]
  (cond
    (string? data)
      (loop [src data
             row 0
             col 0
             tiles (transient {})]
        (if-let [px (first src)]
          (if (= \newline px)
            (recur (rest src) (inc row) 0 tiles)
            (recur (rest src) row (inc col) (assoc! tiles [col row] px)))
          (persistent! tiles)))
    (map? data)
      data
    (coll? data)
      (encode (cs/join "\n" data))))

(defn render
  "Function to render the map as it exists in the argument. The keys are
  [x y] (col, row) - and the values are the contents to display."
  [tiles]
  (let [bts (keys tiles)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))]
    (for [r (range rows)
          :let [rd (for [[k v] tiles :when (= r (second k))] [k v])]]
        (->> (sort-by (comp first first) rd)
             (map second)
             (apply str)))))

(def puzzle
  "This is the input of the paths of the wires for the panel."
  (-> (slurp "resources/2019/input/day18.txt")
      (cs/trim)
      (encode)))

(def trial1
  "Test data for the first part - it takes 8 steps"
  (encode ["#########"
           "#b.A.@.a#"
           "#########"]))

(def trial2
  "Test data for the first part - it takes 86 steps"
  (encode ["########################"
           "#f.D.E.e.C.b.A.@.a.B.c.#"
           "######################.#"
           "#d.....................#"
           "########################"]))

(def trial3
  "Test data for the first part - it takes 132 steps"
  (encode ["########################"
           "#...............b.C.D.f#"
           "#.######################"
           "#.....@.a.B.c.d.A.e.F.g#"
           "########################"]))

(def trial4
  "Test data for the first part - it takes 136 steps"
  (encode ["#################"
           "#i.G..c...e..H.p#"
           "########.########"
           "#j.A..b...f..D.o#"
           "########@########"
           "#k.E..a...g..B.n#"
           "########.########"
           "#l.F..d...h..C.m#"
           "#################"]))

(def trial5
  "Test data for the first part - it takes 81 steps"
  (encode ["########################"
           "#@..............ac.GI.b#"
           "###d#e#f################"
           "###A#B#C################"
           "###g#h#i################"
           "########################"]))

(defn explore
  "Function to explore the surrounding squares of [x y] and see if we can
  move into them - and if so, see where they lead. This is going to be
  called recursively to walk every possible path until we get to one of
  the targets (tgt), and then we'll stop - indicating the path we took to
  get here, and what it is we hit."
  [brd wall tgt stps [x y]]
  (let [mov (for [p [[x (inc y)] [x (dec y)] [(dec x) y] [(inc x) y]]
                  :when (not (or (wall (brd p)) (stps p)))
                  :let [nstps (assoc stps [x y] (count stps))]]
              {:stps nstps :pos p})
        ans (atom [])]
    (doseq [mi mov]
      (if-let [hit (tgt (brd (:pos mi)))]
        (swap! ans conj {:target hit :path (assoc (:stps mi) (:pos mi) (count (:stps mi)))})
        (swap! ans concat (explore brd wall tgt (:stps mi) (:pos mi)))))
    @ans))

(defn unlock
  "Function to 'unlock' a door (uppercase char) with the provided key (lowercase
  char) and remove both from the board. This will return a cleaner version of
  the board, ready for the next step in the processing."
  [brd k]
  (into {}
    (for [[p c] brd]
      (if (or (= c k) (= c (ucase k))) [p \.] [p c]))))

(defn- mrbig
  "Function to look at all possible paths for an exploration, and for each
  target hit, pick the shortest path to that target, but keep all the targets,
  and then recurse on that to get all paths to getting all the targets. This
  will then call itself for each possible path, and return the completed
  path and steps to get there."
  [ans brd pos blk tgt & [cnt kch]]
  (let [bsf (first (sort (map :steps @ans)))
        dist (map #(assoc % :dist (count (:path %))) (explore brd blk tgt {} pos))
        hits (->> (for [[k v] (group-by :target dist)] (first (sort-by :dist v)))
                  (sort-by :dist)
                  ; (filter #(< (:dist %) (or bsf (inc (:dist %)))))
                  )
        ]
    ; (infof "kch: %s hits: %s" (apply str (or kch [])) (pr-str (for [h hits] [(keyword (str (:target h))) (count (:path h))])))
    (if (empty? hits)
      (infof "final kch: %s steps: %d bsf: %d" (apply str (or kch [])) (or cnt 0) (or bsf 0)))
    (if (and (not-empty hits) (or (nil? bsf) (and cnt (< cnt bsf))))
      (doseq [pseg (take 2 hits)
              :let [nbrd (unlock brd (:target pseg))
                    npos (->> (:path pseg)
                              (filter #(= (second %) (dec (:dist pseg))))
                              (first)
                              (first))]]
        ; (infof "[new] board: %s" (pr-str (render nbrd)))
        ; (infof "[new] pos: %s tgt: %s cnt: %d" npos (pr-str (disj tgt (:target pseg))) (+ (or cnt 0) (dec (:dist pseg))))
        (mrbig ans nbrd npos blk (disj tgt (:target pseg)) (+ (or cnt 0) (dec (:dist pseg))) (conj (or kch []) (:target pseg))))
      (swap! ans conj {:pos pos :left tgt :found (or kch []) :steps (or cnt 0)})
      )))

(defn yoyo
  "4318 - to high"
  [& [arg]]
  (let [src (or arg puzzle)
        inp (unlock src \@)
        spos (first (for [[k v] src :when (= v \@)] k))
        blk (conj (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") \#)
        hunt (set "abcdefghijklmnopqrstuvwxyz")
        ans (atom [])
       ]
    (mrbig ans inp spos blk (set (filter hunt (vals inp))))
    ; @ans
    (first (sort-by :steps @ans))
  ))

(defn one
  ""
  []
  )
