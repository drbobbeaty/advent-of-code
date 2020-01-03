(ns advent-of-code.2019.day18
  "Eighteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long ucase lcase sum hr-in-millis]]
            [clojure.core.memoize :as memo]
            [clojure.math.combinatorics :refer [combinations permutations]]
            [clojure.set :refer [union]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse-map
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
      (parse-map (cs/join "\n" data))))

(def puzzle
  "This is the input of the map of the locations of the keys and locks to find
  the shortest bath through."
  (-> (slurp "resources/2019/input/day18p1.txt")
      (cs/trim)
      (parse-map)))

(def quad
  "This is the input of the map of the locations of the keys and locks to find
  the shortest bath through - but with four separate chambers and bots to find
  all the keys."
  (-> (slurp "resources/2019/input/day18p2.txt")
      (cs/trim)
      (parse-map)))

(def trial1
  "Test data for the first part - it takes 8 steps"
  (parse-map ["#########"
              "#b.A.@.a#"
              "#########"]))

(def trial2
  "Test data for the first part - it takes 86 steps"
  (parse-map ["########################"
              "#f.D.E.e.C.b.A.@.a.B.c.#"
              "######################.#"
              "#d.....................#"
              "########################"]))

(def trial3
  "Test data for the first part - it takes 132 steps"
  (parse-map ["########################"
              "#...............b.C.D.f#"
              "#.######################"
              "#.....@.a.B.c.d.A.e.F.g#"
              "########################"]))

(def trial4
  "Test data for the first part - it takes 136 steps"
  (parse-map ["#################"
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
  (parse-map ["########################"
              "#@..............ac.GI.b#"
              "###d#e#f################"
              "###A#B#C################"
              "###g#h#i################"
              "########################"]))

(def trial6
  "Test data for the second part - it takes 24 steps"
  (parse-map ["###############"
              "#d.ABC.#.....a#"
              "######@#@######"
              "###############"
              "######@#@######"
              "#b.....#.....c#"
              "###############"]))

(def trial7
  "Test data for the second part - it takes 32 steps"
  (parse-map ["#############"
              "#DcBa.#.GhKl#"
              "#.###@#@#I###"
              "#e#d#####j#k#"
              "###C#@#@###J#"
              "#fEbA.#.FgHi#"
              "#############"]))

(def trial8
  "Test data for the second part - it takes 72 steps"
  (parse-map ["#############"
              "#g#f.D#..h#l#"
              "#F###e#E###.#"
              "#dCba@#@BcIJ#"
              "#############"
              "#nK.L@#@G...#"
              "#M###N#H###.#"
              "#o#m..#i#jk.#"
              "#############"]))

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
    (sort-by #(count (:path %)) @ans)))

(defn unlock
  "Function to 'unlock' a door (uppercase char) with the provided key (lowercase
  char) and remove both from the board. This will return a cleaner version of
  the board, ready for the next step in the processing."
  [brd k]
  (into {}
    (for [[p c] brd]
      (if (or (= c k) (= c (ucase k))) [p \.] [p c]))))

(defn distances*
  "Function to return a map of all the distances between the keys in the map.
  This ignores all the locks, and the starting position, it just focuses on the
  walls, assuming all the locks are properly handled in order. This is just
  an idealized path distance between each pair of keys."
  [brd]
  (let [aks (filter (set "abcdefghijklmnopqrstuvwxyz") (vals brd))
        als (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]
    (into {}
      (for [[st en] (combinations aks 2)
            :let [spos (first (for [[k v] brd :when (= v st)] k))
                  pth (:path (first (explore brd #{\#} #{en} {} spos)))
                  len (if-not (empty? pth) (dec (count pth)))
                  iip (map brd (keys pth))
                  lks (filter als iip)
                  xks (remove #{st en} (filter (set aks) iip))
                  bar (distinct (concat (lcase lks) xks))]]
        [[st en] {:length len :path pth :locks lks :keys xks :barrier bar}]))))

(def distances
  "Memoized function to return a map of all the distances between the keys in
  the map. This ignores all the locks, and the starting position, it just
  focuses on the walls, assuming all the locks are properly handled in order.
  This is just an idealized path distance between each pair of keys."
  (memo/lru distances* :lru/threshold 2))

(defn all-keys*
  "Function to return a sequence of all the keys present on the provided board.
  This is used for a lot of things, and we'll memoize the function so that it's
  fast and easy to get the same data over and over."
  [brd]
  (filter (set "abcdefghijklmnopqrstuvwxyz") (vals brd)))

(def all-keys
  "Memoized function to return a sequence of all the keys present on the
  provided board. This is used for a lot of things, and we're memoizing the
  function so that it's fast and easy to get the same data over and over."
  (memo/lru all-keys* :lru/threshold 2))

(declare gather)

(defn gather*
  "Function to gather up the path distance on the given board, from the
  provided key capturing all the remaining keys, also provided. This
  looks at the 'distances' on the board, and makes sure it's legal based
  on the lock and key state, and then simply calls this same function for
  parts of the remainder of the path."
  [brd ck rks]
  (if (empty? rks)
    0
    (let [pnk (for [[[a b] {len :length xks :barrier pp :path}] (distances brd)
                    :when (and (or (and (= a ck) (rks b))
                                   (and (= b ck) (rks a)))
                               (not-any? rks xks))]
                [(if (= a ck) b a) len])
          ans (atom nil)]
      (doseq [[nk nd] pnk
              :let [rd (gather brd nk (disj rks nk))]
              :when (some? rd)
              :let [d (+ nd rd)]]
        (if (or (nil? @ans) (< d @ans))
          (reset! ans d)))
      @ans)))

(def gather
  "Memoized function to gather up the path distance on the given board,
  from the provided key capturing all the remaining keys, also provided.
  This looks at the 'distances' on the board, and makes sure it's legal based
  on the lock and key state, and then simply calls this same function for
  parts of the remainder of the path."
  (memo/ttl gather* :ttl/threshold hr-in-millis))

(defn one
  "Function to look at all the distances between the keys, and to note what
  keys are in the way, and what locks are as well. Then we start from the
  initial position, and for each key we can reach, find the additional distance
  from that key, gathering all the remaining keys. The memoization of the
  'gather' function really makes this work well."
  [& [arg]]
  (let [src (or arg puzzle)
        spos (first (for [[k v] src :when (= v \@)] k))
        blk (conj (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") \#)
        aks (all-keys src)
        fp (for [en aks
                 :let [sp (:path (first (explore src (disj blk en) #{en} {} spos)))]
                 :when sp]
             [en (dec (count sp))])]
    (->> (for [[k d] fp] (+ d (gather src k (disj (set aks) k))))
         (sort)
         (first))))

(defn reachable*
  "Function to return all the keys in the board that are reachable from the
  starting position. As some might not be, this is a good way to divide up
  what can be reached from what can't, and simplify the problem considerably."
  [brd pos & [bars]]
  (for [k (all-keys brd)
        :let [sp (:path (first (explore brd (union #{\#} bars) #{k} {} pos)))]
        :when sp]
    [k (dec (count sp))]))

(def reachable
  "Memoized function to return all the keys in the board that are reachable
  from the starting position. As some might not be, this is a good way to
  divide up what can be reached from what can't, and simplify the problem
  considerably."
  (memo/ttl reachable* :ttl/threshold hr-in-millis))

(defn segment
  "Function to look at each starting point, and find all the keys that can
  possibly be collected from that point, and then return that as a map with
  the key being the starting position in the board, and the value being the
  set of keys for that 'section' of the board."
  [brd]
  (into {} (for [spos (for [[k v] brd :when (= v \@)] k)]
             [spos (map first (reachable brd spos))])))

(defn two
  "Function to look at a multi-entrance board, and find out the number of
  steps it will take to get all keys from all sections. This is going to
  segment up the board based on what's reachable from each entry point, and
  then gather up the path distance with the existing function, and then
  just sum them together."
  [& [arg]]
  (let [src (or arg quad)]
    (->> (for [[spos lks] (segment src)
               :let [fp (reachable src spos (set (ucase lks)))
                     gkd (first (sort (for [[k d] fp] (+ d (gather src k (disj (set lks) k))))))]]
           [spos lks fp gkd])
         (map last)
         (sum))))
