(ns validata.util
  "Utility functions")

(defn map-values
  "Returns the map with values 'updated' by f, a function that
  accepts a key and value and returns an 'updated' value."
  [f m]
  (into {} (map (fn [[k v]] [k (f k v)]) m)))

(defn filter-map
  "Returns a map with key/value pairs filtered by predicate f that
  accepts a key and value."
  [pred m]
  (into {} (filter (fn [[k v]] (pred k v)) m)))
