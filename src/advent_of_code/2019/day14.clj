(ns advent-of-code.2019.day14
  "Fourteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn decode
  "Function to decode a line in the string input of the reactions, and turn
  it into a key-value pair suitable for including in a map which is how we
  want to store this data."
  [s]
  (let [rip (fn [ss] (let [[q m] (rest (re-matches #"([0-9]*) (.*)" (cs/trim ss)))]
                       [(parse-int q) (keyword m)]))
        [inp out] (rest (re-matches #"(.*)=>(.*)" s))]
    [(rip out) (map rip (cs/split inp #","))]))

(def puzzle
  "This is the input of the chemical reactions the nanofactory can perform."
  (-> (slurp "resources/2019/input/day14.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map decode) (into {}))))

(def trial1
  "Test data for part 1 - requires 31 ORE"
  (-> ["10 ORE => 10 A"
       "1 ORE => 1 B"
       "7 A, 1 B => 1 C"
       "7 A, 1 C => 1 D"
       "7 A, 1 D => 1 E"
       "7 A, 1 E => 1 FUEL"]
      (->> (map decode) (into {}))))

(def trial2
  "Test dataa for part 1 - requires 165 ORE"
  (-> ["9 ORE => 2 A"
       "8 ORE => 3 B"
       "7 ORE => 5 C"
       "3 A, 4 B => 1 AB"
       "5 B, 7 C => 1 BC"
       "4 C, 1 A => 1 CA"
       "2 AB, 3 BC, 4 CA => 1 FUEL"]
      (->> (map decode) (into {}))))

(def trial3
  "Test data for part 1 - requires 13312 ORE"
  (-> ["157 ORE => 5 NZVS"
       "165 ORE => 6 DCFZ"
       "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
       "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
       "179 ORE => 7 PSHF"
       "177 ORE => 5 HKGWZ"
       "7 DCFZ, 7 PSHF => 2 XJWVT"
       "165 ORE => 2 GPVTF"
       "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]
      (->> (map decode) (into {}))))

(def trial4
  "Test data for part 1 - requires 180697 ORE"
  (-> ["2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
       "17 NVRVD, 3 JNWZP => 8 VPVL"
       "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
       "22 VJHF, 37 MNCFX => 5 FWMGM"
       "139 ORE => 4 NVRVD"
       "144 ORE => 7 JNWZP"
       "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
       "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
       "145 ORE => 6 MNCFX"
       "1 NVRVD => 8 CXFTF"
       "1 VJHF, 6 MNCFX => 4 RFSQX"
       "176 ORE => 6 VJHF"]
      (->> (map decode) (into {}))))

(def trial5
  "Test data for part 1 - requires 2210736 ORE"
  (-> ["171 ORE => 8 CNZTR"
       "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
       "114 ORE => 4 BHXH"
       "14 VRPVC => 6 BMBT"
       "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
       "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
       "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
       "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
       "5 BMBT => 4 WPTQ"
       "189 ORE => 9 KTJDG"
       "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
       "12 VRPVC, 27 CNZTR => 2 XDBXC"
       "15 KTJDG, 12 BHXH => 5 XCVML"
       "3 BHXH, 2 VRPVC => 7 MZWV"
       "121 ORE => 7 VRPVC"
       "7 XCVML => 6 RJRHP"
       "5 BHXH, 4 VRPVC => 5 LTCX"]
      (->> (map decode) (into {}))))

(defn- generate
  "Function to take the element to generate, and do our best to do that -
  either by reaction, or by generating more components. This will get down
  to :ORE, eventually, and that's OK too."
  [elem nano idx ore inven]
  (let [[iq _] (get idx elem)
        dms (get nano [iq elem])
        chk (into {} (for [[k v] dms] [v k]))]
    (if (= (map second dms) [:ORE])
      (do
        ; (infof "generating %d more %s from :ORE" iq elem)
        (swap! ore + (first (first dms)))
        (swap! inven update elem + iq))
      (if-let [miss (not-empty (for [[cq cm] dms :when (< (cm @inven) (cm chk))] cm))]
        (generate (first miss) nano idx ore inven)
        (do
          ; (infof "generating %d more %s from reaction" iq elem)
          (doseq [[cq cm] dms] (swap! inven update cm - cq))
          (swap! inven update elem + iq))))))

(defn one
  "Function to take the puzzle input and compute the minimum amount of :ORE
  that needs to be mined for the creation of 1 unit of :FUEL. This is all in
  the puzzle input, and we generate it a step at a time, and keep track of
  all the ore we have mined, and then just return that at the end."
  [& [{react :reactions sinv :inven sore :ore :as args}]]
  (let [nano (or react puzzle)
        idx (into {} (for [[[kq km] _] nano] [km [kq km]]))
        base (set (for [[k v] nano :when (and (= 1 (count v)) (= :ORE (second (first v))))] (second k)))
        ore (atom (or sore 0))
        inven (atom (or sinv (into {} (for [k (keys idx) :when (not= :FUEL k)] [k 0]))))
        fuel (atom (into {} (for [k (map second (get nano [1 :FUEL]))] [k 0])))
        chk (into {} (for [[iq im] (get nano [1 :FUEL])] [im iq]))]
    (loop [cnt 0]
      ; (info "================================")
      ; (infof "ore:   %d" @ore)
      ; (infof "inven: %s" (pr-str @inven))
      ; (infof "fuel:  %s" (pr-str @fuel))
      ; (infof "check: %s" (pr-str chk))
      ;; update fuel values if we have the inventory to meet the need
      (doseq [[el fi] @fuel]
        (let [ai (el @inven)
              ti (el chk)]
          (if (and (< fi ti) (<= ti ai))
            (let [mva (- ti fi)]
              ; (infof "moving %d of %s from inven to fuel" mva el)
              (swap! inven update el - mva)
              (swap! fuel update el + mva)))))
      ;; find the first element we're short on
      (if-let [nxt (first (for [[el fi] @fuel :when (< fi (el chk))] el))]
        (do
          (generate nxt nano idx ore inven)
          (recur (inc cnt)))
        {:reactions nano :ore @ore :inven @inven}))))

(defn two
  "Function to try and find the fule 1 trillion units of ORE would make, and
  the hope is that there will be a case with zero remainder, and then that
  can be the baseline. Didn't find it... so extrapolated."
  [& [{react :reactions sinv :inven sore :ore :as args}]]
  (loop [fuel 0
         mach (one {:reactions (or react puzzle)})]
    (if (zero? (mod (inc fuel) 2000))
      (infof "fuel: %d ore: %d" (inc fuel) (:ore mach)))
    (if (= [0] (distinct (vals (:inven mach))))
      (warnf "zero inventory! fuel: %d ore: %d" (inc fuel) (:ore mach)))
    (if (< (or (:ore mach) 0) 1000000000000)
      (recur (inc fuel) (one mach))
      fuel)))

(defn bobo
  "3281722
   3281806
   3281807 - too low
   3281817 - wrong
   3281820 - bingo
   3281832 - too high"
  []
  (let [data [{:fuel 500  :ore 152380744}
              {:fuel 1000 :ore 304734352}
              {:fuel 1500 :ore 457091333}
              {:fuel 2000 :ore 609442820}
              {:fuel 2500 :ore 761808966}
              {:fuel 3000 :ore 914154165}
              {:fuel 4000 :ore 1218858634}
              {:fuel 6000 :ore 1828282991}
              {:fuel 8000 :ore 2437708674}
              {:fuel 10000 :ore 3047115048}
              {:fuel 12000 :ore 3656529135}
              {:fuel 14000 :ore 4265942855}
              {:fuel 16000 :ore 4875374498}
              {:fuel 18000 :ore 5484783310}
              {:fuel 20000 :ore 6094203252}
              {:fuel 22000 :ore 6703625396}
              {:fuel 24000 :ore 7313043132}
              {:fuel 26000 :ore 7922465367}
              {:fuel 28000 :ore 8531857396}
              ]
        ul 3281832
        ll 3281807]
    (for [d data
          :let [tgt (int (* (:fuel d) (/ 1000000000000 (:ore d))))]
          :when (< ll tgt ul)]
      tgt)))
