(ns validata.core-test
  (:require [clojure.test :refer :all]
            [validata.core :as v]))

; -------------------
; Test Key Validation
; -------------------

(deftest key-present?-test
  (testing "key-present?"
    (are [x y z] (= (v/key-present? x y {}) z)
         nil   nil   false
         nil   false false
         nil   true  false
         false nil   false
         false false false
         false true  false
         true  nil   true
         true  false true
         true  true  true
         "a"   nil   true
         "a"   false true
         "a"   true  true
         :a    nil   true
         :a    false true
         :a    true  true)))

(deftest key-keyword?-test
  (testing "key-keyword?"
    (are [x y z] (= (v/key-keyword? x y {}) z)
         nil   nil   true
         nil   true  true
         nil   :x    true
         false false false
         false true  false
         false :x    false
         true  nil   false
         true  true  false
         true  :x    false
         :a    nil   true
         :a    true  true
         :a    :x    true
         "a"   nil   false
         "a"   true  false
         "a"   :x    false)))

(deftest key-string?-test
  (testing "key-string?"
    (are [x y z] (= (v/key-string? x y {}) z)
         nil   nil   true
         nil   true  true
         nil   :x    true
         false false false
         false true  false
         false :x    false
         true  nil   false
         true  true  false
         true  :x    false
         :a    nil   false
         :a    true  false
         :a    :x    false
         "a"   nil   true
         "a"   true  true
         "a"   :x    true)))

; ---------------------
; Test Value Validation
; ---------------------

(deftest boolean?-test
  (testing "boolean?"
    (are [k v e] (= (v/boolean? k v {}) e)
         nil nil  true
         :a nil   false
         :a ""    false
         :a false true
         :a true  true)))

(deftest integer?-test
  (testing "integer?"
    (are [k v e] (= (v/integer? k v {}) e)
         nil nil   true
         :a  nil   false
         :a  false false
         :a  "3"   false
         :a  9.9   false
         :a  -1    true
         :a  42    true)))

(deftest keyword?-test
  (testing "keyword?"
    (are [k v e] (= (v/keyword? k v {}) e)
         nil nil   true
         :a  nil   false
         :a  false false
         :a  ""    false
         :a  :b    true)))

(deftest map?-test
  (testing "map?"
    (are [k v e] (= (v/map? k v {}) e)
         nil nil         true
         :a  nil         false
         :a  ""          false
         :a  {}          true
         :a  {:a 1 :b 2} true)))

(deftest not-nil?-test
  (testing "not-nil?"
    (are [k v e] (= (v/not-nil? k v {}) e)
         nil nil    true
         :a  nil    false
         :a  false  true
         :a  "x"    true
         :a  :b     true)))

(deftest number?-test
  (testing "number?"
    (are [k v e] (= (v/number? k v {}) e)
         nil nil   true
         :a  nil   false
         :a  false false
         :a  ""    false
         :a  -3    true
         :a  42    true
         :a  3.4   true)))

(deftest positive?-test
  (testing "positive?"
    (are [k v e] (= (v/positive? k v {}) e)
         nil nil   true
         :a  nil   false
         :a  false false
         :a  ""    false
         :a  -3    false
         :a  42    true
         :a  3.4   true)))

(deftest seq?-test
  (testing "seq?"
    (are [k v e] (= (v/seq? k v {}) e)
         nil nil      true
         :a  nil      false
         :a  false    false
         :a  :b       false
         :a  '()      true
         :a  '(1 2 3) true)))

(deftest set?-test
  (testing "set?"
    (are [k v e] (= (v/set? k v {}) e)
         nil nil      true
         :a  nil      false
         :a  false    false
         :a  :b       false
         :a  #{}      true
         :a  #{1 2 3} true)))

(deftest string?-test
  (testing "string?"
    (are [k v e] (= (v/string? k v {}) e)
         nil nil   true
         :a  nil   false
         :a  false false
         :a  :b    false
         :a  "b"   true)))

(deftest timestamp?-test
  (testing "timestamp?"
    (are [k v e] (= (v/timestamp? k v {}) e)
         nil nil                              true
         :a  nil                              false
         :a  ""                               false
         :a  #inst "2013-05-21"               true
         :a  #inst "2013-05-21T21:04"         true
         :a  #inst "2013-05-21T21:04:54.622Z" true)))

(deftest timestamp-string?-test
  (testing "timestamp-string?"
    (are [k v e] (= (v/timestamp-string? k v {}) e)
         nil nil                        true
         :a  nil                        false
         :a  ""                         false
         :a  "2013-05-32"               false
         :a  "2013-05-02T21:74:54"      false
         :a  "2013-05-02T29:0Q:54.622Z" false
         :a  "2013-05-21"               true
         :a  "2013-05-21T21:04"         true
         :a  "2013-05-21T21:04:54.622Z" true)))

(deftest uuid?-test
  (testing "uuid?"
    (are [k v e] (= (v/uuid? k v {}) e)
         nil nil                                          true
         :a  nil                                          false
         :a  ""                                           false
         :a  "d227317f-96aa-4e9b-a383-7e3a25a7712f"       false
         :a  #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f" true)))

(deftest uuid-string?-test
  (testing "uuid-string?"
    (are [k v e] (= (v/uuid-string? k v {}) e)
         nil nil                                          true
         :a  nil                                          false
         :a  ""                                           false
         :a  "d227317f-96aa-4e9b-a383-7e3a25a7712f"       true
         :a  #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f" false)))

(deftest vector?-test
  (testing "vector?"
    (are [k v e] (= (v/vector? k v {}) e)
         nil nil   true
         :a  nil   false
         :a  ""    false
         :a  []    true
         :a  [1 2] true)))

; -------------------------
; Test Validation Functions
; -------------------------

(deftest property-error
  (testing "property-error"
    (testing "boolean"
      (is (= (:error v/boolean)
             (v/property-error :a nil v/boolean {})))
      (is (= (:error v/boolean)
             (v/property-error :a :b v/boolean {})))
      (is (= nil
             (v/property-error :a true v/boolean {})))
      (is (= nil
             (v/property-error :a false v/boolean {}))))
    (testing "keyword"
      (is (= (:error v/keyword)
             (v/property-error :a nil v/keyword {})))
      (is (= (:error v/keyword)
             (v/property-error :a "b" v/keyword {})))
      (is (= nil
             (v/property-error :a :b  v/keyword {}))))
    (testing "not-nil"
      (is (= (:error v/not-nil)
             (v/property-error :a nil v/not-nil {})))
      (is (= nil
             (v/property-error :a "b" v/not-nil {})))
      (is (= nil
             (v/property-error :a :b v/not-nil {}))))
    (testing "number"
      (is (= (:error v/number)
             (v/property-error :a nil v/number {})))
      (is (= (:error v/number)
             (v/property-error :a :b v/number {})))
      (is (= nil
             (v/property-error :a 3 v/number {})))
      (is (= nil
             (v/property-error :a 3.14 v/number {}))))
    (testing "string"
        (is (= (:error v/string)
               (v/property-error :a nil v/string {})))
        (is (= (:error v/string)
               (v/property-error :a :b  v/string {})))
        (is (= nil
               (v/property-error :a "b" v/string {}))))
    (testing "timestamp"
      (is (= (:error v/timestamp)
             (v/property-error :a nil v/timestamp {})))
      (is (= (:error v/timestamp)
             (v/property-error :a :b  v/timestamp {})))
      (is (= (:error v/timestamp)
             (v/property-error :a "2013-05-01" v/timestamp {})))
      (is (= nil
             (v/property-error :a #inst "2013-05-01" v/timestamp {}))))
    (testing "timestamp-string"
      (is (= (:error v/timestamp-string)
             (v/property-error :a nil v/timestamp-string {})))
      (is (= (:error v/timestamp-string)
             (v/property-error :a :b v/timestamp-string {})))
      (is (= (:error v/timestamp-string)
             (v/property-error :a #inst "2013-05-01" v/timestamp-string {})))
      (is (= nil
             (v/property-error :a "2013-05-01" v/timestamp-string {}))))
    (testing "uuid"
      (is (= (:error v/uuid)
             (v/property-error :a nil v/uuid {})))
      (is (= (:error v/uuid)
             (v/property-error :a :b  v/uuid {})))
      (is (= (:error v/uuid)
             (v/property-error
               :a "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid {})))
      (is (= nil
             (v/property-error
               :a #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid {}))))
    (testing "uuid-string"
      (is (= (:error v/uuid-string)
             (v/property-error :a nil v/uuid-string {})))
      (is (= (:error v/uuid-string)
             (v/property-error :a :b v/uuid-string {})))
      (is (= (:error v/uuid-string)
             (v/property-error
              :a #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f"
              v/uuid-string {})))
      (is (= nil
             (v/property-error
               :a "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid-string {}))))))

(deftest extra-keys-test
  (testing "extra-keys"
    (let [vs {:a [v/string]}]
      (is (= (v/extra-keys {:a 1}      vs) #{}))
      (is (= (v/extra-keys {:a 1 :b 2} vs) #{:b})))))

(deftest extra-keys?-test
  (testing "extra-keys?"
    (let [vs {:a [v/string]}]
      (is (= (v/extra-keys? {:a 1}      vs) false))
      (is (= (v/extra-keys? {:a 1 :b 2} vs) true)))))

(deftest errors-test
  (testing "errors"
    (testing "1 string"
      (let [vs {:a [v/string]}]
        (is (= {:a [(:error v/string)]}
               (v/errors {:a :b} vs)))
        (is (= {}
               (v/errors {} vs)))
        (is (= {}
               (v/errors {:a "b"} vs)))))
    (testing "1 string, 1 keyword"
      (let [vs {:a [v/string]
                :c [v/keyword]}]
        (is (= {:a [(:error v/string)]}
               (v/errors {:a :b} vs)))
        (is (= {:a [(:error v/string)]}
               (v/errors {:a :b :c :d} vs)))
        (is (= {:c [(:error v/keyword)]}
               (v/errors {:c "d"} vs)))
        (is (= {:c [(:error v/keyword)]}
               (v/errors {:c "d" :a "b"} vs)))
        (is (= {}
               (v/errors {} vs)))
        (is (= {}
               (v/errors {:a "b"} vs)))
        (is (= {}
               (v/errors {:c :d} vs)))
        (is (= {}
               (v/errors {:a "b" :c :d} vs)))))
    (testing "1 required string"
      (let [vs {:a [v/string v/required]}]
        (is (= {:a [(:error v/string)]}
               (v/errors {:a :b} vs)))
        (is (= {:a [(:error v/required)]}
               (v/errors {} vs)))
        (is (= {:a [(:error v/string)]}
               (v/errors {:a nil} vs)))
        (is (= {}
               (v/errors {:a "b"} vs)))))
    (testing "disallow unexpected keys"
      (let [vs {:a [v/number v/required]
                :x [v/keyword]}]
        (is (= {:b [:error "key is unexpected"]
                :c [:error "key is unexpected"]}
               (v/errors {:a 1 :b 2 :c 3} vs false)))
        (is (= {:b [:error "key is unexpected"]
                :x [(:error v/keyword)]}
               (v/errors {:a 1 :b 2 :x 42} vs false)))))))

(deftest valid?-test
  (testing "valid?"
    (are [m e] (is (= (v/valid? m {:a [v/string]})))
         {:a :b}  false
         {}       true
         {:a "b"} true)))

(deftest if-valid-test
  (testing "if-valid"
    (let [vs {:a [v/string]}]
      (is (thrown?
            clojure.lang.ExceptionInfo
            (v/if-valid {:a :x} vs identity)))
      (is (v/if-valid {:a "x"} vs identity))
      (is (v/if-valid {:a "x" :b "y"} vs identity))
      (is (thrown?
            clojure.lang.ExceptionInfo
            (v/if-valid {:a "x" :b "y"} vs false identity))))))
