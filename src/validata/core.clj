(ns validata.core
  "Validation functions."
  (:refer-clojure :rename {boolean core-boolean
                           integer? core-integer?
                           keyword core-keyword
                           keyword? core-keyword?
                           number? core-number?
                           string? core-string?})
  (:require [clj-time.format :as time-format]
            [clojure.set :as set]
            [validata.util :as util]))

; ------------------------
; Key Validation Functions
; ------------------------
;
; By convention, all functions that test the key should start with 'key-'.

(defn key-present?
  "Is the key present?"
  [k v & [_]]
  (core-boolean k))

(defn key-keyword?
  "If key not nil, is the key a keyword?"
  [k v & [_]]
  (if (nil? k) true
    (core-keyword? k)))

(defn key-string?
  "If key not nil, is the key a string?"
  [k v & [_]]
  (if (nil? k) true
    (core-string? k)))

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
;
; All functions must:
; 1. Return true when a key is nil. This is by design; use `key-required` if a
;    key is required.
; 2. Return false if a key is present but the value is nil. If you want to
;    signify a missing value, do not pass the key.

(defn boolean?
  "If key not nil, is value a boolean?"
  [k v & [_]]
  (if (nil? k) true
    (or (= v true) (= v false))))

(defn integer?
  "If key not nil, is value an integer?"
  [k v & [_]]
  (if (nil? k) true
    (core-integer? v)))

(defn keyword?
  "If key not nil, is value a keyword?"
  [k v & [_]]
  (if (nil? k) true
    (core-keyword? v)))

(defn not-nil?
  "If key not nil, is value not nil?"
  [k v & [_]]
  (if (nil? k) true
    (not (nil? v))))

(defn number?
  "If key not nil, is value a number?"
  [k v & [_]]
  (if (nil? k) true
    (core-number? v)))

(defn positive?
  "If key not nil, is value positive?"
  [k v & [_]]
  (if (nil? k) true
    (if (not (number? k v)) false
      (pos? v))))

(defn string?
  "If key not nil, is value a string?"
  [k v & [_]]
  (if (nil? k) true
    (core-string? v)))

(defn timestamp?
  "If key not nil, is value a timestamp?"
  [k v & [_]]
  (if (nil? k) true
    (= java.util.Date (type v))))

(defn timestamp-string?
  "If key not nil, is value a timestamp string?"
  [k v & [_]]
  (if (nil? k) true
    (if (not (string? k v)) false
      (core-boolean (time-format/parse v)))))

(defn uuid?
  "If key not nil, is value a uuid?"
  [k v & [_]]
  (if (nil? k) true
    (= java.util.UUID (type v))))

(def uuid-re
  (let [groups (map #(str "[0-9a-fA-F]{" % "}") [8 4 4 4 12])]
    (re-pattern (clojure.string/join "-" groups))))

(defn uuid-string?
  "If key not nil, is value a uuid string?"
  [k v & [_]]
  (if (nil? k) true
    (if (not (string? k v)) false
      (core-boolean (re-matches uuid-re v)))))

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

(def not-nil
  {:validator not-nil?
   :error "value must be non-nil"})

(def number
  {:validator number?
   :error "value must be a number"})

(def positive
  {:validator positive?
   :error "value must be positive"})

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

; ----------------------------
; Private Validation Functions
; ----------------------------

(defn property-error
  "Validate a property (a key and value) against a validation. Returns error
  or nil."
  [k v validation props]
  (if (apply (:validator validation) [k v props])
    nil
    (:error validation)))

(defn property-errors
  "Validate a property (a key and value) against validations. Returns a vector
  of errors or []."
  [k v validations props]
  (filterv identity (map #(property-error k v % props) validations)))

;(defn extra-key-error)

(defn extra-keys
  "Returns the set of extra keys in map that are not present in
  validation-map."
  [m validation-map]
  (set/difference (set (keys m)) (set (keys validation-map))))

(defn extra-keys?
  "Does map include keys that are not listed in validation-map?"
  [m validation-map]
  (not (empty? (extra-keys m validation-map))))

; -------------------------------
; High-Level Validation Functions
; -------------------------------

(defn errors-for-expected-keys
  "Validate the map using validations. Returns a map of failures, if any."
  [m validation-map]
  (let [errors-fn
        (fn [k v]
          (property-errors (or (some #{k} (keys m)) (if (vector? k) k)) (get m k) (get validation-map k) m))
        value-not-empty? (fn [k v] (not (empty? v)))]
    (->> validation-map
         (util/map-values errors-fn)
         (util/filter-map value-not-empty?))))

(defn errors-for-unexpected-keys
  "Validate the map using validations. Returns a map of failures, if any."
  [m validation-map]
  (let [e-keys (extra-keys m validation-map)
        error-fn (fn [k] {k [:error "key is unexpected"]})]
    (into {} (map error-fn e-keys))))

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
     (throw (validata.Exception. (str (errors m validations))))))
  ([m validations allow-extra-keys? f]
   (if (valid? m validations allow-extra-keys?)
     (f m)
     (throw (validata.Exception. (str (errors m validations)))))))
