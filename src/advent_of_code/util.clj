(ns advent-of-code.util
  "These functions are simple utility functions that we can use in any of the
  day's puzzles. They are here to simply afford simpler re-use."
  (require [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def not-nil? (complement nil?))

(def ascii-0 (int \0))
(def ascii-9 (int \9))
(def ascii-punct #{ \( \) \. \; })

(defn rev
  "Function to do a simple reverse sort versus `compare`. This is for a simple
  descending sort order - nothing special."
  [a b]
  (* -1 (compare a b)))

(defn is-digit?
  "Predicate function to return 'true' if the supplied character is an ASCII
  digit: 0-9. This is helpful in scanning string data using filters."
  [c]
  (<= ascii-0 (int c) ascii-9))

(defn is-int-char?
  "Predicate function to return 'true' if the supplied character is a valid
  ASCII character for an int: 0-9 or '-'. This is helpful in scanning string
  data using filters."
  [c]
  (or (is-digit? c) (= \- c)))

(defn is-double-char?
  "Predicate function to return 'true' if the supplied character is a valid
  ASCII character for a double: 0-9, '.', '-'. This is helpful in scanning
  string data using filters."
  [c]
  (or (is-int-char? c) (= \. c)))

(defn is-punct?
  "Predicate function to return 'true' if the supplied character is an ASCII
  punctuation. This is helpful in scanning string data using filters."
  [c]
  (ascii-punct c))

(defn is-ascii?
  "Predicate function to return `true` is the supplied character is a *printable*
  ASCII character. That means no control codes, and no Unicode characters."
  [c]
  (<= 32 (int c) 127))

(defn nil-if-empty
  "Function to ensure that if we have an empty string, map or collection,
  it's converted to a nil so that there are no empty strings, maps or
  collections in the data sets. These really shouldn't be there, but they
  sneak in from time to time."
  [arg]
  (cond
    (nil? arg) nil
    (or (string? arg) (map? arg) (coll? arg)) (if-not (empty? arg) arg)
    :else arg))

(defn nil-if-zero
  "Function to ensure that if we have a zero, it's converted to a nil so that
  there are no zeros in the data sets. These are most likely invalid parsing
  valus from the data, as we need to nil them out as opposed to passing on
  bad data."
  [arg]
  (cond
    (nil? arg) nil
    (and (number? arg) (zero? arg)) nil
    :else arg))

(defn non-zero?
  "Function to make sure all args are numbers and not zero. This will be very
  useful in the other functions to make sure that we don't divide by zero, etc."
  [& args]
  (every? #(and (number? %) (not (zero? %))) args))

(defn non-neg?
  "Function to make sure all args are numbers and not negative. This will be
  very useful in the other functions to make sure that we don't have negative
  numbers for numeric args, etc."
  [& args]
  (every? #(and (number? %) (not (neg? %))) args))

(defn ucase
  "Function to convert the string argument to it's upper-case equivalent - but
  do so in a clean manner. This means that if it's a `nil`, you get back a `nil`,
  and if it's a number, you get that number back. This was left out of the spec
  in the original, and it makes for code like this to clean things up. If you
  pass in a collection, this function will call itself on all the values in that
  collection so you can upper-case a collection without worrying about the other
  types of values."
  [s]
  (cond
    (string? s) (cs/upper-case s)
    (coll? s)   (map ucase s)
    :else       s))

(defn lcase
  "Function to convert the string argument to it's lower-case equivalent - but
  do so in a clean manner. This means that if it's a `nil`, you get back a `nil`,
  and if it's a number, you get that number back. This was left out of the spec
  in the original, and it makes for code like this to clean things up. If you
  pass in a collection, this function will call itself on all the values in that
  collection so you can lower-case a collection without worrying about the other
  types of values."
  [s]
  (cond
    (string? s) (cs/lower-case s)
    (coll? s)   (map lcase s)
    :else       s))

(defn trim
  "Function to trim the white space off the the string argument - but
  do so in a clean manner. This means that if it's a `nil`, you get back a `nil`,
  and if it's a number, you get that number back. This was left out of the spec
  in the original, and it makes for code like this to clean things up. If you
  pass in a collection, this function will call itself on all the values in that
  collection so you can lower-case a collection without worrying about the other
  types of values."
  [s]
  (cond
    (string? s) (cs/trim s)
    (coll? s)   (map trim s)
    :else       s))

(defn parse-bool
  "Parses a string into a boolean, expecting \"Inf\" for infinity. A nil is
  parsed as false by default. The default can be overriden by passing a named
  argument, e.g. `(parse-bool val :default true)`."
  [x & {:keys [default] :or {default false}}]
  (cond
    (nil? x) default
    (or (= "NA" x) (= "Inf" x) (= "Infinity" x)) true
    (or (= "-Inf" x) (= "-Infinity" x)) true
    (string? x) (case (lcase x)
                  ("true" "t" "yes" "y" "on" "1") true
                  ("false" "f" "no" "n" "off" "0") false
                  default)
    (coll? x) (map parse-bool x)
    (number? x) (not (zero? x))
    :else x))

(defn parse-int
  "Parses a string into an int, expecting \"Inf\" for infinity. A nil is
  parsed as 0 by default - similar to ruby's `to_i` method. The default can
  be overriden by passing a named argument,\n  e.g. `(parse-int val
  :default true)`."
  [x & {:keys [default] :or {default 0}}]
  (cond
    (nil? x) default
    (or (= "NA" x) (= "Inf" x) (= "Infinity" x)) Integer/MAX_VALUE
    (or (= "-Inf" x) (= "-Infinity" x)) Integer/MIN_VALUE
    (string? x) (cond
                  (empty? x) default
                  (some #(not (is-int-char? %)) x) default
                  :else (try
                          (Integer/parseInt (cs/trim x))
                          (catch java.lang.NumberFormatException nfe
                            (infof "Unable to parse '%s' into an integer!" x)
                            default)))
    (coll? x) (map parse-int x)
    (number? x) (int (if (pos? x) (min x Integer/MAX_VALUE) (max x Integer/MIN_VALUE)))
    :else x))

(defn parse-long
  "Function to parse a long from a string - but it might not be a string
  and it might be null, and it might have whitespace - so I have to trim it
  and then parse it, but a null at any point is going to be disaster. So
  this function makes sure it's properly conditioned before parsing."
  [x & {:keys [default] :or {default 0}}]
  (cond
    (nil? x) default
    (or (= "NA" x) (= "Inf" x) (= "Infinity" x)) Long/MAX_VALUE
    (or (= "-Inf" x) (= "-Infinity" x)) Long/MIN_VALUE
    (string? x) (cond
                  (empty? x) default
                  (some #(not (is-int-char? %)) x) default
                  (< 19 (count x)) default
                  :else (try
                          (Long/parseLong (cs/trim x))
                          (catch java.lang.NumberFormatException nfe
                            (infof "Unable to parse '%s' into a long!" x)
                            default)))
    (coll? x) (map parse-long x)
    (number? x) (long (if (pos? x) (min x Long/MAX_VALUE) (max x Long/MIN_VALUE)))
    :else x))

(defn parse-double
  "Parses a string into a double, expecting \"Inf\" for infinity. A nil is
  parsed as 0 by default - similar to ruby's `to_f` method. The default can
  be overriden by passing a named argument,\n  e.g. `(parse-double val
  :default nil)`."
  [x & {:keys [default] :or {default 0.0}}]
  (cond
    (nil? x) default
    (or (= "NA" x) (= "Inf" x) (= "Infinity" x)) Double/POSITIVE_INFINITY
    (or (= "-Inf" x) (= "-Infinity" x)) Double/NEGATIVE_INFINITY
    (string? x) (cond
                  (empty? x) default
                  (some #(not (is-double-char? %)) x) default
                  :else (try
                          (Double/parseDouble (cs/trim x))
                          (catch java.lang.NumberFormatException nfe
                            (infof "Unable to parse '%s' into a double!" x)
                            default)))
    (coll? x) (map parse-double x)
    (number? x) (double x)
    :else x))

(defn parse-money
  "Parses a string into a double, expecting \"Inf\" for infinity. A nil is
  parsed as 0 by default - similar to ruby's `to_f` method. The default can
  be overriden by passing a named argument, e.g. `(parse-money val :default
  nil)`. This will strip the `$` and `,` so that we don't have to worry
  about those messing up the parsing."
  [x & {:keys [default] :or {default 0}}]
  (cond
    (string? x) (parse-double (apply str (filter is-double-char? x)) :default default)
    (coll? x)   (map #(parse-money % :default default) x)
    :else       (parse-double x :default default)))
