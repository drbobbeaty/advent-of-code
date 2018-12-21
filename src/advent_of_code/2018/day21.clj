(ns advent-of-code.2018.day21
  "Twenty-first day's solutions for the Advent of Code 2018"
  (require [advent-of-code.2016.day25 :refer [->int]]
           [advent-of-code.2018.day16 :refer [addr addi mulr muli banr bani borr
                                              bori setr seti gtir gtri gtrr eqir
                                              eqri eqrr]]
           [advent-of-code.2018.day19 :refer [operations op-names code-it run]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the program from the wrist unit:

       #ip 4             ; set ip to R4
       ;; initial testing section of the code to make sure 'bani' works
       seti 123 0 3      ; R3 = 123
       bani 3 456 3      ; R3 = R3 | 456
       eqri 3 72 3       ; R3 = (R3 == 72 ? 1 : 0)  - if (R3 == 72)
       addr 3 4 4        ; R4(ip) = R4(ip) + R3     /   SKIP NEXT
       seti 0 0 4        ; R4(ip) = 0              /    GOTO 0
       ;;
       seti 0 2 3        ; R3 = 0
     6 bori 3 65536 2    ; R2 = R3 | 65536          - R2 is at least 65536
       seti 1397714 1 3  ; R3 = 1397714
     8 bani 2 255 5      ; R5 = R2 & 255            - R5 = R2 & 0xff
       addr 3 5 3        ; R3 = R3 + R5             - R3 += R2 & 0xff
       bani 3 16777215 3 ; R3 = R3 & 16777215       - R3 &= 0xffffff (24 bits)
       muli 3 65899 3    ; R3 = R3 * 65899          - R3 *= 65899
       bani 3 16777215 3 ; R3 = R3 & 16777215       - R3 &= 0xffffff (24 bits)
       gtir 256 2 5      ; R5 = (256 > R2 ? 1 : 0)
       ;; at this point the registers are: [x 0 65536 1039046 13 0]
                                           [x 1   256 4273858 13 1]

    14 addr 5 4 4        ; R4(ip) = R4(ip) + R5     - JMPR (R5+1)
       addi 4 1 4        ; R4(ip) += 1              - SKIP NEXT
       seti 27 6 4       ; R4(ip) = 27              - GOTO 28

       ;; divide R2 by 256 and place in R5
       seti 0 6 5        ; R5 = 0
    18 addi 5 1 1        ; R1 = R5 + 1
       muli 1 256 1      ; R1 = R1 * 256
       gtrr 1 2 1        ; R1 = (R1 > R2 ? 1 : 0)
       addr 1 4 4        ; R4(ip) = R4(ip) + R1     - JMPR (R1+1)
       addi 4 1 4        ; R4(ip) = R4(ip) + 1      - SKIP NEXT
       seti 25 2 4       ; R4(ip) = 25              - GOTO 26
       addi 5 1 5        ; R5 = R5 + 1
       seti 17 0 4       ; R4(ip) = 17              - GOTO 18

       ;; R2 is R2 / 256 at this point
    26 setr 5 7 2        ; R2 = R5
       seti 7 4 4        ; R4(ip) = 7               - GOTO 8

    28 eqrr 3 0 5        ; R5 = (R3 == R0 ? 1 : 0)  - if (R3 == R0)
       addr 5 4 4        ; R4(ip) = R4(ip) + R5     -   EXIT
       seti 5 8 4        ; R4(ip) = 5               -   GOTO 6

  from previous puzzles, it makes sense to look at what it's doing in
  order to solve the puzzle."
  (-> (slurp "resources/2018/input/day21.txt")
      (cs/split #"\n")))

(def sample
  "This is the beginning of the program to test the bitwise operations"
  ["#ip 4"
   "seti 123 0 3"
   "bani 3 456 3"
   "eqri 3 72 3"
   "addr 3 4 4"
   "seti 0 0 4"
   "seti 0 2 3"
   "bori 3 65536 2"
   "seti 1397714 1 3"
   "bani 2 255 5"
   "addr 3 5 3"
   "bani 3 16777215 3"
   "muli 3 65899 3"
   "bani 3 16777215 3"
   "gtir 256 2 5"
])

(defn one
  "Function to run the background process with the minimum value possible in
  register 0 to get the code to complete as quickly as possible."
  []
  (let [raw puzzle
        ip (->> (re-matches #"^#ip (\d+)$" (first raw))
             (second)
             (->int))
        pgm (code-it (drop 1 raw))
        reg (atom [3909249 0 0 0 0 0])]
    (run reg pgm ip 0)))

(defn dupes
  "Function to run a program, with a stargin set of registers, and an IP addr,
  and starting program counter, and run it all the way to it's completion -
  returning the registers when done."
  [reg pgm ip spc]
  (let [reg2 (atom #{})
        old (atom [])
        reg3 (atom #{})
        tot (loop [pc spc
                   cnt 0]
              (if-let [row (nth pgm pc nil)]
                (let [[ins a b c] row]
                  (swap! reg assoc ip pc)
                  (ins reg a b c)
                  (when (= 13 pc)
                    (when (@reg3 (nth @reg 3))
                      (infof "ip=%s : %s %s %s %s : %s :: last: %s" pc (get op-names ins) a b c @reg @old)
                      (throw (Exception. "Stopping!"))
                      )
                    (swap! reg3 conj (nth @reg 3))
                    (reset! old @reg)
                    ; (if (@reg2 (nth @reg 2))
                    ;   (throw (Exception. "Stopping!")))
                    ; (swap! reg2 conj (nth @reg 2))
                  )
                  (recur (inc (nth @reg ip)) (inc cnt)))
                cnt))]
    {:reg @reg :steps tot}))

(defn two
  "Function to run the background process with the minimum value possible in
  register 0 to get the code to complete - but take as long as possible."
  []
  (let [raw puzzle
        ip (->> (re-matches #"^#ip (\d+)$" (first raw))
             (second)
             (->int))
        pgm (code-it (drop 1 raw))
        reg (atom [0 0 0 0 0 0])]
    (dupes reg pgm ip 0)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [raw sample
        ip (->> (re-matches #"^#ip (\d+)$" (first raw))
             (second)
             (->int))
        pgm (code-it (drop 1 raw))
        reg (atom [0 0 0 0 0 0])]
    (run reg pgm ip 0)))
