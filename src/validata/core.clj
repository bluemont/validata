(ns validata.core
  "Validation functions."
  (:refer-clojure :rename {boolean core-boolean
                           integer? core-integer?
                           keyword core-keyword
                           keyword? core-keyword?
                           number? core-number?
                           string? core-string?})
  (:require [clj-time.format :as time-format]
            [validata.util :as util]))

; ------------------------
; Key Validation Functions
; ------------------------
;
; By convention, all functions that test the key should start with 'key-'.

(defn key-present?
  "Is the key present?"
  [k v]
  (core-boolean k))

(defn key-keyword?
  "If key not nil, is the key a keyword?"
  [k v]
  (if (nil? k) true
    (core-keyword? k)))

(defn key-string?
  "If key not nil, is the key a string?"
  [k v]
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
  [k v]
  (if (nil? k) true
    (or (= v true) (= v false))))

(defn integer?
  "If key not nil, is value an integer?"
  [k v]
  (if (nil? k) true
    (core-integer? v)))

(defn keyword?
  "If key not nil, is value a keyword?"
  [k v]
  (if (nil? k) true
    (core-keyword? v)))

(defn not-nil?
  "If key not nil, is value not nil?"
  [k v]
  (if (nil? k) true
    (not (nil? v))))

(defn number?
  "If key not nil, is value a number?"
  [k v]
  (if (nil? k) true
    (core-number? v)))

(defn positive?
  "If key not nil, is value positive?"
  [k v]
  (if (nil? k) true
    (if (not (number? k v)) false
      (pos? v))))

(defn string?
  "If key not nil, is value a string?"
  [k v]
  (if (nil? k) true
    (core-string? v)))

(defn timestamp?
  "If key not nil, is value a timestamp?"
  [k v]
  (if (nil? k) true
    (= java.util.Date (type v))))

(defn timestamp-string?
  "If key not nil, is value a timestamp string?"
  [k v]
  (if (nil? k) true
    (if (not (string? k v)) false
      (core-boolean (time-format/parse v)))))

(defn uuid?
  "If key not nil, is value a uuid?"
  [k v]
  (if (nil? k) true
    (= java.util.UUID (type v))))

(def uuid-re
  (let [groups (map #(str "[0-9a-fA-F]{" % "}") [8 4 4 4 12])]
    (re-pattern (clojure.string/join "-" groups))))

(defn uuid-string?
  "If key not nil, is value a uuid string?"
  [k v]
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
  [k v validation]
  (if (apply (:validator validation) [k v])
    nil
    (:error validation)))

(defn property-errors
  "Validate a property (a key and value) against validations. Returns a vector
  of errors or []."
  [k v validations]
  (filterv identity (map #(property-error k v %) validations)))

; -------------------------------
; High-Level Validation Functions
; -------------------------------

(defn errors
  "Validate the map using validations. Returns a map of failures, if any."
  [m validation-map]
  (let [errors-fn
        (fn [k v]
          (property-errors (some #{k} (keys m)) (k m) (k validation-map)))
        value-not-empty? (fn [k v] (not (empty? v)))]
    (->> validation-map
         (util/map-values errors-fn)
         (util/filter-map value-not-empty?))))

(defn valid?
  "Is the map valid for the given validations?"
  [m validation-map]
  (empty? (errors m validation-map)))

(defn if-valid
  "If valid, call function with map; otherwise, throw exception."
  [m validations f]
  (if (valid? m validations)
    (f m)
    (throw (Exception. (str (errors m validations))))))
