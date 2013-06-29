(ns validata.util-test
  (:require [clojure.test :refer :all]
            [validata.util :as u]))

(deftest map-values-test
  (testing "map-values"
    (are [x y z] (= (u/map-values x y) z)
         (fn [k v] k)       {:a 1 :b 2} {:a :a :b :b}
         #((constantly %2)) {:a 1 :b 2} {:a 1 :b 2}
         (fn [k v] (inc v)) {:a 1 :b 2} {:a 2 :b 3})))

(deftest filter-map-test
  (testing "filter-map"
    (are [x y z] (= (u/filter-map x y) z)
         #(odd? %2)           {:a 1 :b 2 :c 3} {:a 1 :c 3}
         (fn [k v] (even? v)) {:a 1 :b 2 :c 3} {:b 2})))
