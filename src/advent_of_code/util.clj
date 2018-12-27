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

(defn update-keys
  "Like update (above), but operates on multiple keys at once. Note
  this is different from update-in (though they have the same argument
  signature), which uses the list of keys as a path into the nested
  data structure.  For update-keys, all the keys are top-level."
  [m ks f & args]
  (reduce #(apply update %1 %2 f args)
          m
          ks))

(defn update-existing-keys
  "Similar to update-keys (above), but operates only on existing keys
  in the map. Note this is different from update-in (though they have
  the same argument signature), which uses the list of keys as a path
  into the a nested data structure. For update-existing-keys, all the
  keys are top-level, and ignored if they aren't present."
  [m ks f & args]
  (reduce #(if (contains? %1 %2) (apply update %1 %2 f args) %1)
          m
          ks))

(defn update-all
  "Function to apply the provided function to *all* values in the provided
  map. The result is a map with all the values changed by the function."
  [m f & args]
  (reduce-kv (fn [m' k v] (assoc m' k (apply f v args))) {} m))

(defn rename-keys
  "This is completely compatible with `clojure.set/rename-keys`.
  But in addition to that behavior,
    - If the second arg is a map and the value of a mapping is a function then the
    function will be called with the key and the result will be used as the new key.
    - If the second arg is a function then all the keys of `map` will be transformed
    by it.
    - If the second arg is sequential then only the keys in the sequence will be renamed
    according to the third argument, assumed to be a function (identity is used if
    there is not third argument)."
  [map & [km_f_s f]]
  (let [kmap (cond
               (map? km_f_s) km_f_s
               (fn? km_f_s) (zipmap (keys map) (repeat km_f_s))
               (sequential? km_f_s) (zipmap km_f_s (repeat (or f identity)))
               :else {km_f_s (or f identity)})]
    (reduce
      (fn [m [old new]]
        (let [fnew (if (fn? new) new (fn [_] new))]
          (if (contains? map old)
            (assoc m (fnew old) (get map old))
            m)))
      (apply dissoc map (keys kmap)) kmap)))

(defn remove-nil-keys
  "Given a map, returns a new map with all keys whose values are nil removed."
  [m]
  (reduce (fn [m [k v]] (if (nil? v) (dissoc m k) m)) m m))

(defn compact
  "Simple convenience function to remove the nils from a collection, or
  nil- or empty-valued keys from a map, but leave everything else alone.
  This is just like the ruby compact method, and it's really quite useful for
  sums and operations that will choke on nils."
  [s]
  (cond
    (map? s)  (-> (update-all s nil-if-empty)
                  (remove-nil-keys))
    (set? s)  (disj s nil)
    (coll? s) (remove nil? s)
    :else     s))

(defn compact-to-nil
  "Function to remove all nils in a sequence and returns nil if the sequence
  returns as empty."
  [s]
  (if-let [cs (compact s)]
    (if-not (empty? cs) cs)))

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

(defn sum
  "Function to sum a collection, or a single value, and we need this because
  many of the redis library commands will return a single value OR a sequence
  and we need to be able to handle either case easily."
  [x]
  (cond
    (nil? x) 0
    (string? x) (if (neg? (.indexOf x ".")) (parse-int x) (parse-double x))
    (map? x) (sum (vals x))
    (coll? x)
      (reduce + (map sum (compact x)))
    :else x))