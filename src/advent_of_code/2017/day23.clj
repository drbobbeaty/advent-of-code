(ns advent-of-code.2017.day23
  "Twenty-third day's solution for part 1 for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int is-int-char? is-prime?]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the skip-factor for my spinlock."
  (-> (slurp "resources/2017/input/day23.txt")
    (cs/trim)
    (cs/split #"\n")))

(def sample
  "This is first part of the code - and we're using it to verify the code."
  ["set b 93"
   "set c b"
   "jnz a 2"
   "jnz 1 5"
   "mul b 100"
   "sub b -100000"
   "set c b"
   "sub c -17000"])

(defn rget
  "Function to get the value of the register, if the argument is a register,
  or return the number, if the value is a number. This will make the work in
  each instruction simpler."
  [regs v]
  (cond
    (number? v)  v
    (keyword? v) (get @regs v)))

(defn seti
  "Function that sets register X to the value of Y."
  [regs x y]
  (swap! regs assoc x (rget regs y))
  1)

(defn add
  "Function that increases register X by the value of Y."
  [regs x y]
  (swap! regs update x + (rget regs y))
  1)

(defn sub
  "Function that decreases register X by the value of Y."
  [regs x y]
  (swap! regs update x - (rget regs y))
  1)

(def ^:private mul-count (atom 0))

(defn mul
  "Function that sets register X to the result of multiplying the value
  contained in register X by the value of Y"
  [regs x y]
  (swap! mul-count inc)
  (swap! regs update x * (rget regs y))
  1)

(defn jnz
  "Function that jumps with an offset of the value of Y, but only if the
  value of X is not zero. (An offset of 2 skips the next
  instruction, an offset of -1 jumps to the previous instruction, and
  so on.)"
  [regs x y]
  (if-not (zero? (rget regs x))
    (rget regs y)
    1))

(defn code-it
  "Function to take the puzzle input and parse it into something we can
  really use easily in the execution of this sound board."
  [s]
  (let [key-or-val (fn [x] (if (every? is-int-char? x) (parse-int x) (keyword x)))]
    (cond
      (string? s) (cond
                    (.startsWith s "set")
                      (let [args (drop 1 (re-matches #"^set (.+) (.+)$" s))]
                        {:inst seti :args (mapv key-or-val args)})
                    (.startsWith s "add")
                      (let [args (drop 1 (re-matches #"^add (.+) (.+)$" s))]
                        {:inst add :args (mapv key-or-val args)})
                    (.startsWith s "sub")
                      (let [args (drop 1 (re-matches #"^sub (.+) (.+)$" s))]
                        {:inst sub :args (mapv key-or-val args)})
                    (.startsWith s "mul")
                      (let [args (drop 1 (re-matches #"^mul (.+) (.+)$" s))]
                        {:inst mul :args (mapv key-or-val args)})
                    (.startsWith s "jnz")
                      (let [args (drop 1 (re-matches #"^jnz (.+) (.+)$" s))]
                        {:inst jnz :args (mapv key-or-val args)}))
      (coll? s)   (mapv code-it s)
      :else       s)))

(defn init-reg
  "Function to take a program, find all the registers in use in the program,
  and create an initial map of registers populated with zeros, so that we can
  start off with this and run as needed."
  [pgm]
  (->> (map :args pgm)
    (apply concat)
    (filter keyword?)
    (set)
    (map (fn [k] [k 0]))
    (into {})))

(defn run
  "Function to run a program with an optional set of registers, and log a
  few important milestones in the process. When the program is finished,
  we'll return the register map for analysis."
  [pgms & [rs]]
  (let [reg (atom (or rs (init-reg pgms)))]
    (loop [pc 0]
      (if-let [i (nth pgms pc nil)]
        (recur (+ pc (apply (:inst i) (concat [reg] (:args i)))))
        @reg))))

(defn one
  "Function to run the puzzle program - as-is - and see how many times the
  'mul' operation is called. Not bad at all..."
  []
  (-> (run (code-it puzzle))
    (assoc :mul-cnt @mul-count)))

(defn two
  "Function to calculate the ending value of the 'h' register in the program,
  and we had to disassemble it:

    set b 93       ; B = 93
    set c b        ; C = B
    jnz a 2        ; if A == 0   -----------------+
    jnz 1 5        ;                              |
    mul b 100      ; B = 9300 = B * 100           |
    sub b -100000  ; B = 109300 = B - (-100000)   |
    set c b        ; C = 109300 = B               |
    sub c -17000   ; C = 126300 += 17000          |
    set f 1        ; F = 1       <------------+---+
    set d 2        ; D = 2                    |
    set e 2        ; E = 2            <----+  |
    set g d        ; G = (D * E) - B  <-+  |  |
    mul g e        ;                    |  |  |        ; G *= E
    sub g b        ;                    |  |  |        ; G -= B
    jnz g 2        ; if G = 0           |  |  |
    set f 0        ;   F = 0            |  |  |
    sub e -1       ; E += 1             |  |  |
    set g e        ; G = E - B          |  |  |
    sub g b        ;                    |  |  |        ; G -= B
    jnz g -8       ; if G != 0  --------+  |  |
    sub d -1       ; D += 1                |  |
    set g d        ; G = D - B             |  |
    sub g b        ;                       |  |        ; G -= B
    jnz g -13      ; if G != 0  -----------+  |
    jnz f 2        ; if F == 0                |
    sub h -1       ;   H += 1                 |
    set g b        ; G = B - C                |
    sub g c        ;                          |        ; G -= C
    jnz g 2        ; if G == 0                |
    jnz 1 3        ;   EXIT                   |
    sub b -17      ; B += 17                  |
    jnz 1 -23      ; JUMP     ----------------+

  Where it's running D & E from 2 to B, to see if any values under B are
  factors of B, and then it sets F to 0, and that increments H. So it's
  counting the non-primes between the starting B, and 1000 steps of 17."
  []
  (loop [b 109300
         npc 0
         cnt 0]
    (if (<= cnt 1000)
      (recur (+ b 17) (if-not (is-prime? b) (inc npc) npc) (inc cnt))
      {:non-primes npc})))

(defn yoyo
  "Function to test the components as we are building up things. That's it."
  []
  (run (code-it puzzle)))
