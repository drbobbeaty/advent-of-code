(ns advent-of-code.2018.day23
  "Twenty-third day's solutions for the Advent of Code 2018"
  (require [advent-of-code.2016.day25 :refer [->int]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the location and radius data for all of the nanobots."
  (-> (slurp "resources/2018/input/day23.txt")
      (cs/trim)
      (cs/split #"\n")))

(def sample
  "This is the sample nanobot data for testing."
  ["pos=<0,0,0>, r=4"
   "pos=<1,0,0>, r=1"
   "pos=<4,0,0>, r=3"
   "pos=<0,2,0>, r=1"
   "pos=<0,5,0>, r=3"
   "pos=<0,0,3>, r=1"
   "pos=<1,1,1>, r=1"
   "pos=<1,1,2>, r=1"
   "pos=<1,3,1>, r=1"])

(def cluster
  "This is "
  ["pos=<10,12,12>, r=2"
   "pos=<12,14,12>, r=2"
   "pos=<16,12,12>, r=4"
   "pos=<14,14,14>, r=6"
   "pos=<50,50,50>, r=200"
   "pos=<10,10,10>, r=5"])

(defn code-it
  "Function to take the string data of the puzzle definition, and parse it into
  a usable data structure for processing."
  [s]
  (let [fix (fn [[x y z r]] {:pos [x y z] :radius r})]
    (cond
      (string? s) (->> (re-matches #"^pos=<(-{0,1}\d+),(-{0,1}\d+),(-{0,1}\d+)>, r=(\d+)$" s)
                    (drop 1)
                    (map ->int)
                    (fix))
      (coll? s)   (map code-it s)
      :else       s)))

(defn rev
  "Function to do a simple reverse sort versus `compare`. This is for a simple
  descending sort order - nothing special."
  [a b]
  (* -1 (compare a b)))

(defn dist
  "Function to compute the Manhattan distance between the two points
  provided. This is just the simple distance in the coordinates."
  [p rp]
  (let [d (fn [[a b]] (Math/abs (- a b)))]
    (->> (map vector p rp)
      (map d)
      (apply +))))

(defn most-neighbors
  "Function to look at all the nanobots provided, and return the one that
  itself has the greatest number of neighbors around it."
  [& [src]]
  (let [nbs (code-it (or src puzzle))
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))]
    (->> (for [nb nbs
               :let [irc (count (filter #(rng? (:pos nb) %) nbs))]
               :when (pos? irc)]
           (assoc nb :count irc))
      (sort-by :count >)
      (first))))

(defn grid-search
  "This is trying "
  [[xl xh] [yl yh] [zl zh] dx]
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))]
    (->> (for [z (range zl zh dx)
               y (range yl yh dx)
               x (range xl xh dx)
              :let [tp [x y z]
                    irc (count (filter #(rng? tp %) nbs))]
              :when (pos? irc)]
           {:pos tp :count irc :dist (dist [0 0 0] tp) :dx dx})
      (sort-by :count >)
      (take 10)))
  )

(defn search
  ""
  []
  (let [nbs (code-it puzzle)
        orx (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        ory (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        orz (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        rmin (int (Math/floor (/ (apply min (map :radius nbs)) 4)))
        ; srch (fn [rx ry rz dx]
        ;        (->> (for [{cnt :count d :dist p :pos dx :dx :as gg} (grid-search rx ry rz dx)
        ;                  ]
        ;               (grid-search rx' ry' rz' dx'))
        ;          (apply concat)
        ;          (sort-by :count >)
        ;          (take 10)
        ;        ))
        ]
    (->> (for [{cnt :count d :dist [x y z] :pos dx :dx :as gg} (grid-search orx ory orz rmin)
              ]
           (grid-search [(- x dx) (+ x dx)] [(- y dx) (+ y dx)] [(- z dx) (+ z dx)] (Math/floor (/ dx 20))))
      (apply concat)
      (sort-by :count >)
      (take 10)
    )
    ; (loop [bps (grid-search orx ory orz rmin)
    ;        dx rmin]
    ;   (if-let [fsr (first bps)]
    ;     )
    ; )
  ))

(defn rough-grid
  "This is trying to do a searching function over the space of all nanobots.
  We are going to basically grid the space by min(radius)/2 so that we don't
  miss anything, and we'll just let it go. When we get a number from this
  we can look to refine it."
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        stps (fn [[lo hi] dx] (Math/ceil (/ (- hi lo) dx)))
        [xl xh] (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        [yl yh] (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        [zl zh] (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        rmin (int (Math/floor (/ (apply min (map :radius nbs)) 4)))]
    (->> (for [z (range zl zh rmin)
               y (range yl yh rmin)
               x (range xl xh rmin)
              :let [tp [x y z]
                    irc (count (filter #(rng? tp %) nbs))]
              :when (pos? irc)]
           {:pos tp :count irc :dist (dist [0 0 0] tp) :dx rmin})
      (sort-by :count >)
      (take 10)
      )
    ))

(defn one
  "Function to calculate the number of nanobots within the range of the
  strongest field of one nanobot."
  []
  (let [nbs (code-it puzzle)
        {rp :pos sr :radius} (first (sort-by :radius > nbs))]
    (count (filter #(<= (dist rp (:pos %)) sr) nbs))))

(defn two
  ""
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        [xl xh] (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        [yl yh] (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        [zl zh] (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        fo2-diff (/ (loop [d 1] (if (< d (- xh xl)) (recur (* 2 d)) d)) 2)
        ]
    (->> (for [z (range zl (inc zh))
               y (range yl (inc yh))
               x (range xl (inc xh))
               :let [tp [x y z]
                     irc (count (filter #(rng? tp %) nbs))]
               :when (pos? irc)]
           {:pos tp :count irc})
      (sort-by :count >)
      (first)
      (:pos)
      (dist [0 0 0])
      )
    ))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [nbs (code-it sample)
        {rp :pos sr :radius} (first (sort-by :radius > nbs))]
    (count (filter #(<= (dist rp (:pos %)) sr) nbs))))

(defn gogo
  "This is trying to do a searching function over the space of all nanobots.
  For this data, the nanobot that has the most neighbors is:

    {:count 848 :pos [18417524 22039426 36968152] :radius 97840933}

  so let's see if we can do something with that."
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        jmp 100000
        stp 25000]
    (loop [[bgx bgy bgz] [18417524 22039426 36968152]
          ]
      (let [[xl xh] [(- bgx jmp) (inc (+ bgx jmp))]
            [yl yh] [(- bgy jmp) (inc (+ bgy jmp))]
            [zl zh] [(- bgz jmp) (inc (+ bgz jmp))]
            best (->> (for [x (range xl xh stp)
                            y (range yl yh stp)
                            z (range zl zh stp)
                           :let [tp [x y z]
                                 irc (count (filter #(rng? tp %) nbs))]
                           :when (pos? irc)]
                        {:pos tp :count irc :dist (dist [0 0 0] tp)})
                   (sort-by (juxt :count :dist) rev)
                   (first))
           ]
        (infof "best: %s" best)
        (recur (:pos best))
        ))))

(defn bobo
  "This is trying to do a searching function over the space of all nanobots.
  This works for the 'cluster' data - in the example, and for the real data
  it's stuck at:

    {:count 893 :dist 77852209 :pos [13210274 27101304 37540631]}

  but AoC says that's too low. Likely, the real maximum count is > 893."
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        stps (fn [[lo hi] dx] (Math/ceil (/ (- hi lo) dx)))
        [xl xh] (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        [yl yh] (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        [zl zh] (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        rmin (int (Math/floor (/ (apply min (map :radius nbs)) 4)))
        bg (fn [rx ry rz js] (let [dx (min rmin js)
                                   xs (inc (stps rx dx))
                                   ys (inc (stps ry dx))
                                   zs (inc (stps rz dx))
                                   calc (for [z (map #(+ (first rz) (* dx %)) (range zs))
                                              y (map #(+ (first ry) (* dx %)) (range ys))
                                              x (map #(+ (first rx) (* dx %)) (range xs))
                                             :let [tp [x y z]
                                                   irc (count (filter #(rng? tp %) nbs))]
                                             :when (pos? irc)]
                                          {:pos tp :count irc :dist (dist [0 0 0] tp)})
                                   hcnt (apply max (map :count calc))]
                               (->> (filter #(= hcnt (:count %)) calc)
                                 (sort-by :dist)
                                 (first))))]
    (->> (loop [jmp (/ (loop [d 1] (if (< d (- xh xl)) (recur (* 2 d)) d)) 4)
                rx [(int (Math/floor (* 0.9 xl))) (int (Math/ceil (* 0.9 xh)))]
                ry [(int (Math/floor (* 0.9 yl))) (int (Math/ceil (* 0.9 yh)))]
                rz [(int (Math/floor (* 0.9 zl))) (int (Math/ceil (* 0.9 zh)))]]
           (let [{[rpx rpy rpz] :pos cnt :count :as nb} (bg rx ry rz jmp)
                 dj (* 2 jmp)]
             (infof "jmp: %s ... nb: %s" jmp nb)
             (if (< 1 jmp)
               (recur (/ jmp 2) [(- rpx dj) (+ rpx dj)] [(- rpy dj) (+ rpy dj)] [(- rpz dj) (+ rpz dj)])
               nb))))))

(defn refine
  "Function to take what appears to be a local maximum, and look in the near
  neighborhood to see if there is something *else* in this region. So far,
  this has not shown a different maximum."
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        [bgx bgy bgz] [13210274 27101304 37540631]
        [xl xh] [(- bgx 20) (inc (+ bgx 20))]
        [yl yh] [(- bgy 20) (inc (+ bgy 20))]
        [zl zh] [(- bgz 20) (inc (+ bgz 20))]]
    (->> (for [x (range xl xh 2)
               y (range yl yh 2)
               z (range zl zh 2)
              :let [tp [x y z]
                    irc (count (filter #(rng? tp %) nbs))]
              :when (pos? irc)]
           {:pos tp :count irc :dist (dist [0 0 0] tp)})
      (sort-by :count >))))
