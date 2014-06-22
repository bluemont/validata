(ns validata.core
  "Validation functions.

  All validation functions must accept these three arguments --
  key, value, properties -- even if the function does not use some
  of the arguments. This convention allows for function composition.

  There are two kinds of validation functions:

    A. Key Validation Functions

       These are functions that test the key, not the value. By
       convention, there names must begin with 'key-'.

    B. Value Validation Functions

       These are functions that test the value. All must:

       1. Return true when a key is nil. This is by design; use
         `key-required` if a key is required.
       2. Return false if a key is present but the value is nil. To
          signify a missing value, do not pass the key."
  (:refer-clojure :rename {boolean core-boolean
                           integer? core-integer?
                           keyword core-keyword
                           keyword? core-keyword?
                           map core-map
                           map? core-map?
                           number? core-number?
                           seq core-seq
                           seq? core-seq?
                           set core-set
                           set? core-set?
                           string? core-string?
                           vector core-vector
                           vector? core-vector?})
  (:require [clj-time.format :as time-format]
            [clojure.set :as set]
            [validata.util :as util]))

; ------------------------
; Key Validation Functions
; ------------------------

(defn key-present?
  "Is the key present?"
  [k v props]
  (core-boolean k))

(defn key-keyword?
  "If key not nil, is the key a keyword?"
  [k v props]
  (or (nil? k) (core-keyword? k)))

(defn key-string?
  "If key not nil, is the key a string?"
  [k v props]
  (or (nil? k) (core-string? k)))

; -------------------
; Key Validation Vars
; -------------------
;
; These vars contain an associated validation function and an error message.

(def required
  {:validator key-present?
   :error "key is required"})

(def key-keyword
  {:validator key-keyword?
   :error "key must be a keyword"})

(def key-string
  {:validator key-string?
   :error "key must be a string"})

; --------------------------
; Value Validation Functions
; --------------------------

(defn boolean?
  "If key not nil, is value a boolean?"
  [k v props]
  (or (nil? k) (= v true) (= v false)))

(defn integer?
  "If key not nil, is value an integer?"
  [k v props]
  (or (nil? k) (core-integer? v)))

(defn keyword?
  "If key not nil, is value a keyword?"
  [k v props]
  (or (nil? k) (core-keyword? v)))

(defn map?
  "If key not nil, is value a map?"
  [k v props]
  (or (nil? k) (core-map? v)))

(defn not-nil?
  "If key not nil, is value not nil?"
  [k v props]
  (or (nil? k) (not (nil? v))))

(defn number?
  "If key not nil, is value a number?"
  [k v props]
  (or (nil? k) (core-number? v)))

(defn positive?
  "If key not nil, is value positive?"
  [k v props]
  (or (nil? k) (and (core-number? v) (pos? v))))

(defn seq?
  "If key not nil, is value a seq?"
  [k v props]
  (or (nil? k) (core-seq? v)))

(defn set?
  "If key not nil, is value a set?"
  [k v props]
  (or (nil? k) (core-set? v)))

(defn string?
  "If key not nil, is value a string?"
  [k v props]
  (or (nil? k) (core-string? v)))

(defn timestamp?
  "If key not nil, is value a timestamp?"
  [k v props]
  (or (nil? k) (= java.util.Date (type v))))

(defn timestamp-string?
  "If key not nil, is value a timestamp string?"
  [k v props]
  (or (nil? k) (and (core-string? v)
                    (core-boolean (time-format/parse v)))))

(defn uuid?
  "If key not nil, is value a uuid?"
  [k v props]
  (or (nil? k) (= java.util.UUID (type v))))

(def uuid-re
  (let [groups (core-map #(str "[0-9a-fA-F]{" % "}") [8 4 4 4 12])]
    (re-pattern (clojure.string/join "-" groups))))

(defn uuid-string?
  "If key not nil, is value a uuid string?"
  [k v props]
  (or (nil? k) (and (core-string? v)
                    (core-boolean (re-matches uuid-re v)))))

(defn vector?
  "If key not nil, is value a vector?"
  [k v props]
  (or (nil? k) (core-vector? v)))

; ---------------------
; Value Validation Vars
; ---------------------

(def boolean
  {:validator boolean?
   :error "value must be a boolean"})

(def keyword
  {:validator keyword?
   :error "value must be a keyword"})

(def integer
  {:validator integer?
   :error "value must be an integer"})

(def map
  {:validator map?
   :error "value must be a map"})

(def not-nil
  {:validator not-nil?
   :error "value must be non-nil"})

(def number
  {:validator number?
   :error "value must be a number"})

(def positive
  {:validator positive?
   :error "value must be positive"})

(def seq
  {:validator seq?
   :error "value must be a seq"})

(def set
  {:validator set?
   :error "value must be a set"})

(def string
  {:validator string?
   :error "value must be a string"})

(def timestamp
  {:validator timestamp?
   :error "value must be a timestamp"})

(def timestamp-string
  {:validator timestamp-string?
   :error "value must be a timestamp string"})

(def uuid
  {:validator uuid?
   :error "value must be a uuid"})

(def uuid-string
  {:validator uuid-string?
   :error "value must be a uuid string"})

(def vector
  {:validator vector?
   :error "value must be a vector"})

; ----------------------------
; Private Validation Functions
; ----------------------------

(defn property-error
  "Validate a property (a key and value) against a validation. Returns error
  or nil."
  [k v validation props]
  (if ((:validator validation) k v props)
    nil
    (:error validation)))

(defn property-errors
  "Validate a property (a key and value) against validations. Returns a vector
  of errors or []."
  [k v validations props]
  (->> (core-map #(property-error k v % props) validations)
       (filterv identity)))

(defn extra-keys
  "Returns the set of extra keys in map `m` that are not present in
  validation map `v-map`."
  [m v-map]
  (set/difference (core-set (keys m)) (core-set (keys v-map))))

(defn extra-keys?
  "Does map `m` include keys that are not listed in validation-map?"
  [m v-map]
  (not (empty? (extra-keys m v-map))))

; -------------------------------
; High-Level Validation Functions
; -------------------------------

(defn errors-for-expected-keys
  "Validate the map using validations. Returns a map of failures, if any."
  [m validation-map]
  (let [errors-fn
        (fn [k v] (property-errors (or (some #{k} (keys m))
                                       (if (core-vector? k) k))
                                   (get m k) (get validation-map k) m))
        value-not-empty? (fn [k v] (not (empty? v)))]
    (->> validation-map
         (util/map-values errors-fn)
         (util/filter-map value-not-empty?))))

(defn errors-for-unexpected-keys
  "Validate the map using validations. Returns a map of failures, if any."
  [m validation-map]
  (let [e-keys (extra-keys m validation-map)
        error-fn (fn [k] {k [:error "key is unexpected"]})]
    (into {} (core-map error-fn e-keys))))

(defn errors
  "Validate the map using validations. Returns a map of failures, if any.
  Set allow-extra-keys?, an optional third parameter, to false to cause a
  failure if unspecified keys are given."
  ([m validation-map]
   (errors m validation-map true))
  ([m validation-map allow-extra-keys?]
   (let [expected (errors-for-expected-keys m validation-map)]
     (if allow-extra-keys?
       expected
       (let [unexpected (errors-for-unexpected-keys m validation-map)]
         (merge expected unexpected))))))

(defn valid?
  "Is the map valid for the given validations?"
  ([m validation-map]
   (empty? (errors m validation-map)))
  ([m validation-map allow-extra-keys?]
   (empty? (errors m validation-map allow-extra-keys?))))

(defn if-valid
  "If valid, call function with map; otherwise, throw exception."
  ([m validations f]
   (if (valid? m validations)
     (f m)
     (let [errors (errors m validations)]
       (throw (ex-info (str errors) errors)))))
  ([m validations allow-extra-keys? f]
   (if (valid? m validations allow-extra-keys?)
     (f m)
     (let [errors (errors m validations allow-extra-keys?)]
       (throw (ex-info (str errors) errors))))))
