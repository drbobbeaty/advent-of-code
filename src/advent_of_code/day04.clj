(ns advent-of-code.day04
  (:require [pandect.algo.md5 :refer [md5]]))

(defn coin
  "Function to find the first (lowest) number `n` such that the supplied key
  and `n` have an md5 hash that starts with supplied number of zeros. Default
  is 5"
  [sk & [zc]]
  (let [zp (.substring "00000000000000000000000000000000" 0 (or zc 5))]
    (first (for [n (range)
                 :let [ac (md5 (str sk n))]
                 :when (.startsWith ac zp)]
             n))))
