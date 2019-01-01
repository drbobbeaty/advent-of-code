(ns advent-of-code.2017.day16
  "Sixteenth day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(declare code-it)

(def puzzle
  "These are the dance moves for the programs in today's puzzle."
  (-> (slurp "resources/2017/input/day16.txt")
    (cs/trim)
    (cs/split #",")
    (code-it)))

(def sample
  "These are the sample dance moves for just fives dancers."
  (-> ["s1" "x3/4" "pe/b"]
    (code-it)))

(defn spin
  "Spin, written sX, makes X programs move from the end to the front, but
  maintain their order otherwise. (For example, s3 on abcde produces cdeab)."
  [pgms x]
  (let [[fp bp] (split-at (- (count pgms) x) pgms)]
    (vec (concat bp fp))))

(defn exch
  "Exchange, written xA/B, makes the programs at positions A and B swap
  places."
  [pgms a b]
  (let [t (nth pgms b)]
    (-> pgms
      (assoc b (nth pgms a))
      (assoc a t))))

(defn part
  "Partner, written pA/B, makes the programs named A and B swap places."
  [pgms a b]
  (exch pgms (.indexOf pgms a) (.indexOf pgms b)))

(defn code-it
  "Function to take the input from the puzzle and parse it into something that
  is a lot easier to work with. In this case, we're using functions as the
  actions, again, just to make it a little easer."
  [s]
  (cond
    (string? s) (cond
                  (.startsWith s "s")
                    (let [[x] (drop 1 (re-matches #"^s(\d+)$" s))]
                      {:action spin :args [(parse-int x)]})
                  (.startsWith s "x")
                    (let [[a b] (drop 1 (re-matches #"^x(\d+)/(\d+)$" s))]
                      {:action exch :args [(parse-int a) (parse-int b)]})
                  (.startsWith s "p")
                    (let [[a b] (drop 1 (re-matches #"^p(\w+)/(\w+)$" s))]
                      {:action part :args [(first a) (first b)]}))
    (coll? s)   (mapv code-it s)
    :else       s))

(defn dance
  "Function to take a sequence of program dancers, and their dance moves, and
  dances them through their dance, and returns them as they are at the end of
  the dance."
  [pgms dms]
  (loop [ps (vec pgms)
         ds dms]
    (if-let [dm (first ds)]
      (recur (apply (:action dm) (concat [ps] (:args dm))) (rest ds))
      ps)))

(defn one
  "Function to let the programs dance, once, through their dance, and then
  return what their positions are at the end of the dance."
  []
  (apply str (dance "abcdefghijklmnop" puzzle)))

(defn hoe-down
  "Function to run through a large number of dances, the output of each
  feeding the input to the next, and then logging each. This will be scanned
  for a pattern, as there is no way to generate a billion of these in a
  reasonable time."
  []
  (let [dms puzzle]
    (loop [pgms "abcdefghijklmnop"
           cnt 0]
      (let [nxt (dance pgms dms)
            nc (inc cnt)]
        (infof "dance: %s ... '%s'" nc (apply str nxt))
        (if (< cnt 1000)
          (recur nxt nc)
          pgms)))))

(defn two
  "Function to find the value of any number of dances using the pattern that
  exists in the outcome of the dances. The pattern is a 60-dance cycle, and we
  simply have to `mod` the value, and look-up the answer."
  []
  (let [idx (mod 1000000000 60)
        sdr  ["abcdefghijklmnop"
              "pkgnhomelfdibjac"
              "eoahnkljfipdcgbm"
              "cgijbakldmfneohp"
              "ipcgjnfblhkemado"
              "lamknphecfdbgioj"
              "mghpfknjadiolebc"
              "ibkcmadhfongeljp"
              "pdnibohglekjmfca"
              "jpbanilemgdochkf"
              "lgocdkajbpfnhmie"
              "ijdgpanbkcomeflh"
              "albdephcojkgmifn"
              "nkhicfmegpdoljab"
              "efacikgjpondbhlm"
              "bhojlakgdmpiefcn"
              "onbhjiplgckemadf"
              "gamkincebpdlhofj"
              "mhcnpkijadofgelb"
              "olkbmadcpfihegjn"
              "ndiolfchgekjmpba"
              "jnlaiogemhdfbckp"
              "ghfbdkajlnpicmoe"
              "ojdhnailkbfmepgc"
              "agldencbfjkhmopi"
              "ikcobpmehndfgjal"
              "epabokhjnfidlcgm"
              "lcfjgakhdmnoepbi"
              "filcjonghbkemadp"
              "hamkoibelndgcfpj"
              "mcbinkojadfphegl"
              "fgklmadbnpocehji"
              "idofgpbchekjmnla"
              "jigaofhemcdplbkn"
              "hcpldkajginobmfe"
              "fjdciaogklpmenhb"
              "ahgdeiblpjkcmfno"
              "okbflnmecidphjag"
              "enalfkcjipodgbhm"
              "gbpjhakcdmifenlo"
              "pogbjfihclkemadn"
              "camkfolegidhbpnj"
              "mbloikfjadpncehg"
              "phkgmadlinfbecjo"
              "odfphnlbcekjmiga"
              "johafpcembdnglki"
              "cbngdkajhoiflmpe"
              "pjdboafhkgnmeicl"
              "achdeolgnjkbmpif"
              "fklpgimebodncjah"
              "eiagpkbjonfdhlcm"
              "hlnjcakbdmopeigf"
              "nfhljpocbgkemadi"
              "bamkpfgehodclnij"
              "mlgfokpjadnibech"
              "nckhmadgoiplebjf"
              "fdpnciglbekjmoha"
              "jfcapnbemldihgko"
              "blihdkajcfopgmne"
              "njdlfapckhimeobg"]]
    (nth sdr idx)))

