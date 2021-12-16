(ns advent-of-code.2021.day16
  "Sixteenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def hex
  "Map to help convert from hex to binary for the decoding of the packet data."
  {\0 [0 0 0 0] \1 [0 0 0 1] \2 [0 0 1 0] \3 [0 0 1 1] \4 [0 1 0 0]
   \5 [0 1 0 1] \6 [0 1 1 0] \7 [0 1 1 1] \8 [1 0 0 0] \9 [1 0 0 1]
   \A [1 0 1 0] \B [1 0 1 1] \C [1 1 0 0] \D [1 1 0 1] \E [1 1 1 0] \F [1 1 1 1]})

(defn hexbin
  "Function to take a string of hex digits and convert them to a sequence of
  1s and 0s which can then more easily be processed."
  [s]
  (apply concat (map #(get hex %) s)))

(def puzzle
  "This is the input of the hex byte stream of the packet from the elves on the
  ship."
  (-> (slurp "resources/2021/input/day16.txt")
    (trim)))

(def test1
  "Test data for the first part."
  ["D2FE28"
   "38006F45291200"
   "EE00D40C823060"
   "8A004A801A8002F478"
   "620080001611562C8802118E34"
   "C0015000016115A2E0802F182340"
   "A0016C880162017C3686B18A3D4780"])

(defn bindec
  "Function to take a binary sequence and turn it into a decimal number."
  [bs]
  (loop [bits (reverse bs)
         base 1
         sum 0]
    (if-let [b (first bits)]
      (recur (rest bits) (* 2 base) (+ sum (* base b)))
      sum)))

(defn literal
  "Function to decode the payload of a literal packet and return it in the
  standard form of a value and the rest of the unparsed bits."
  [bs]
  (loop [bits bs
         vbts []]
    (let [[c & vb] (take 5 bits)]
      (if (zero? c)
        {:value (bindec (concat vbts vb)) :rest (drop 5 bits)}
        (recur (drop 5 bits) (concat vbts vb))))))

(defn decode
  "Function to take a bit stream, sequence of 1s and 0s, and then parse it
  into the packet, with sub-packets, that the structure allows for."
  [bs]
  (let [ver (bindec (take 3 bs))
        typ (bindec (take 3 (drop 3 bs)))
        body (drop 6 bs)]
    (cond
      (= 4 typ)
        (let [{val :value r :rest} (literal body)]
          {:ver ver :typ typ :body val :rest r})
      :else
        (if (zero? (first body))
          (let [sz (bindec (take 15 (rest body)))]
            (loop [bits (take sz (drop 16 body))
                   subpkts []]
              (let [payload (decode bits)]
                (if (pos? (count (:rest payload)))
                  (recur (:rest payload) (conj subpkts (dissoc payload :rest)))
                  {:ver ver
                   :typ typ
                   :body (conj subpkts (dissoc payload :rest))
                   :rest (drop (+ 16 sz) body)}))))
          (loop [bits (drop 12 body)
                 cnt (bindec (take 11 (rest body)))
                 subpkts []]
            (if (pos? cnt)
              (let [payload (decode bits)]
                (recur (:rest payload) (dec cnt) (conj subpkts (dissoc payload :rest))))
              {:ver ver
               :typ typ
               :body subpkts
               :rest bits}))))))

(defn vercnt
  "Function to recursively sum up all the versions of all the packets in the
  provided pack - including the enclosing packet."
  [pkt]
  (cond
    (coll? (:body pkt))
      (+ (:ver pkt) (sum (map vercnt (:body pkt))))
    :else
      (:ver pkt)))

(defn one
  "Function to find the sum of all the versions on all the packets in the
  input hex byte stream."
  [& [coll]]
  (vercnt (decode (hexbin puzzle))))

(defn workit
  "Function to take a decoded packet, with all the sub-packets in the :body,
  and work out what the value is based on the operator indicated by :typ.
  These are all pretty simple, as it's all just recursive operations."
  [pkt]
  (case (:typ pkt)
    0 (apply + (map workit (:body pkt)))
    1 (apply * (map workit (:body pkt)))
    2 (apply min (map workit (:body pkt)))
    3 (apply max (map workit (:body pkt)))
    4 (:body pkt)
    5 (if (apply > (map workit (:body pkt))) 1 0)
    6 (if (apply < (map workit (:body pkt))) 1 0)
    7 (if (apply = (map workit (:body pkt))) 1 0)))

(defn two
  "Function to find the top-level value of the packet after it's been decoded
  and after it's been operated on."
  [& [coll]]
  (workit (decode (hexbin puzzle))))
