(ns validata.core-test
  (:require [clojure.test :refer :all]
            [validata.core :as v]))

; -------------------
; Test Key Validation
; -------------------

(deftest key-present?-test
  (testing "key-present?"
    (is (= false (v/key-present? nil   nil)))
    (is (= false (v/key-present? nil   false)))
    (is (= false (v/key-present? nil   true)))
    (is (= false (v/key-present? false nil)))
    (is (= false (v/key-present? false false)))
    (is (= false (v/key-present? false true)))
    (is (= true  (v/key-present? true  nil)))
    (is (= true  (v/key-present? true  false)))
    (is (= true  (v/key-present? true  true)))
    (is (= true  (v/key-present? "a"   nil)))
    (is (= true  (v/key-present? "a"   false)))
    (is (= true  (v/key-present? "a"   true)))
    (is (= true  (v/key-present? :a    nil)))
    (is (= true  (v/key-present? :a    false)))
    (is (= true  (v/key-present? :a    true)))))

(deftest key-keyword?-test
  (testing "key-keyword?"
    (is (= true  (v/key-keyword? nil   nil)))
    (is (= true  (v/key-keyword? nil   true)))
    (is (= true  (v/key-keyword? nil   :x)))
    (is (= false (v/key-keyword? false false)))
    (is (= false (v/key-keyword? false true)))
    (is (= false (v/key-keyword? false :x)))
    (is (= false (v/key-keyword? true  nil)))
    (is (= false (v/key-keyword? true  true)))
    (is (= false (v/key-keyword? true  :x)))
    (is (= true  (v/key-keyword? :a    nil)))
    (is (= true  (v/key-keyword? :a    true)))
    (is (= true  (v/key-keyword? :a    :x)))
    (is (= false (v/key-keyword? "a"   nil)))
    (is (= false (v/key-keyword? "a"   true)))
    (is (= false (v/key-keyword? "a"   :x)))))

(deftest key-string?-test
  (testing "key-string?"
    (is (= true  (v/key-string? nil   nil)))
    (is (= true  (v/key-string? nil   true)))
    (is (= true  (v/key-string? nil   :x)))
    (is (= false (v/key-string? false false)))
    (is (= false (v/key-string? false true)))
    (is (= false (v/key-string? false :x)))
    (is (= false (v/key-string? true  nil)))
    (is (= false (v/key-string? true  true)))
    (is (= false (v/key-string? true  :x)))
    (is (= false (v/key-string? :a    nil)))
    (is (= false (v/key-string? :a    true)))
    (is (= false (v/key-string? :a    :x)))
    (is (= true  (v/key-string? "a"   nil)))
    (is (= true  (v/key-string? "a"   true)))
    (is (= true  (v/key-string? "a"   :x)))))

; ---------------------
; Test Value Validation
; ---------------------

(deftest boolean?-test
  (testing "boolean?"
    (is (= true  (v/boolean? nil nil)))
    (is (= false (v/boolean? :a nil)))
    (is (= false (v/boolean? :a "")))
    (is (= true  (v/boolean? :a false)))
    (is (= true  (v/boolean? :a true)))))

(deftest integer?-test
  (testing "integer?"
    (is (= true  (v/integer? nil nil)))
    (is (= false (v/integer? :a nil)))
    (is (= false (v/integer? :a false)))
    (is (= false (v/integer? :a "3")))
    (is (= false (v/integer? :a 9.9)))
    (is (= true  (v/integer? :a -1)))
    (is (= true  (v/integer? :a 42)))))

(deftest keyword?-test
  (testing "keyword?"
    (is (= true  (v/keyword? nil nil)))
    (is (= false (v/keyword? :a nil)))
    (is (= false (v/keyword? :a false)))
    (is (= false (v/keyword? :a "")))
    (is (= true  (v/keyword? :a :b)))))

(deftest not-nil?-test
  (testing "number?"
    (is (= true  (v/not-nil? nil nil)))
    (is (= false (v/not-nil? :a nil)))
    (is (= true  (v/not-nil? :a false)))
    (is (= true  (v/not-nil? :a "x")))
    (is (= true  (v/not-nil? :a :b)))))

(deftest number?-test
  (testing "number?"
    (is (= true  (v/number? nil nil)))
    (is (= false (v/number? :a nil)))
    (is (= false (v/number? :a false)))
    (is (= false (v/number? :a "")))
    (is (= true  (v/number? :a -3)))
    (is (= true  (v/number? :a 42)))
    (is (= true  (v/number? :a 3.4)))))

(deftest positive?-test
  (testing "positive?"
    (is (= true  (v/positive? nil nil)))
    (is (= false (v/positive? :a nil)))
    (is (= false (v/positive? :a false)))
    (is (= false (v/positive? :a "")))
    (is (= false (v/positive? :a -2.5)))
    (is (= true  (v/positive? :a 3.14)))
    (is (= true  (v/positive? :a 42)))))

(deftest string?-test
  (testing "string?"
    (is (= true  (v/string? nil nil)))
    (is (= false (v/string? :a nil)))
    (is (= false (v/string? :a false)))
    (is (= false (v/string? :a :b)))
    (is (= true  (v/string? :a "b")))))

(deftest timestamp?-test
  (testing "timestamp?"
    (is (= true  (v/timestamp? nil nil)))
    (is (= false (v/timestamp? :a nil)))
    (is (= false (v/timestamp? :a "")))
    (is (= true  (v/timestamp? :a #inst "2013-05-21")))
    (is (= true  (v/timestamp? :a #inst "2013-05-21T21:04")))
    (is (= true  (v/timestamp? :a #inst "2013-05-21T21:04:54.622Z")))))

(deftest timestamp-string?-test
  (testing "timestamp-string?"
    (is (= true  (v/timestamp-string? nil nil)))
    (is (= false (v/timestamp-string? :a nil)))
    (is (= false (v/timestamp-string? :a "")))
    (is (= false (v/timestamp-string? :a "2013-05-32")))
    (is (= false (v/timestamp-string? :a "2013-05-02T21:74:54")))
    (is (= false (v/timestamp-string? :a "2013-05-02T29:0Q:54.622Z")))
    (is (= true  (v/timestamp-string? :a "2013-05-21")))
    (is (= true  (v/timestamp-string? :a "2013-05-21T21:04")))
    (is (= true  (v/timestamp-string? :a "2013-05-21T21:04:54.622Z")))))

(deftest uuid?-test
  (testing "uuid?"
    (is (= true  (v/uuid? nil nil)))
    (is (= false (v/uuid? :a nil)))
    (is (= false (v/uuid? :a "")))
    (is (= false (v/uuid? :a "d227317f-96aa-4e9b-a383-7e3a25a7712f")))
    (is (= true  (v/uuid? :a #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f")))))

(deftest uuid-string?-test
  (testing "uuid-string?"
    (is (= true  (v/uuid-string? nil nil)))
    (is (= false (v/uuid-string? :a nil)))
    (is (= false (v/uuid-string? :a "")))
    (is (= false (v/uuid-string?
                   :a #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f")))
    (is (= true (v/uuid-string? :a "d227317f-96aa-4e9b-a383-7e3a25a7712f")))))

; -------------------------
; Test Validation Functions
; -------------------------

(deftest property-error
  (testing "property-error"
    (testing "boolean"
      (is (= (:error v/boolean)
             (v/property-error :a nil v/boolean)))
      (is (= (:error v/boolean)
             (v/property-error :a :b v/boolean)))
      (is (= nil
             (v/property-error :a true v/boolean)))
      (is (= nil
             (v/property-error :a false v/boolean))))
    (testing "keyword"
      (is (= (:error v/keyword)
             (v/property-error :a nil v/keyword)))
      (is (= (:error v/keyword)
             (v/property-error :a "b" v/keyword)))
      (is (= nil
             (v/property-error :a :b  v/keyword))))
    (testing "not-nil"
      (is (= (:error v/not-nil)
             (v/property-error :a nil v/not-nil)))
      (is (= nil
             (v/property-error :a "b" v/not-nil)))
      (is (= nil
             (v/property-error :a :b v/not-nil))))
    (testing "number"
      (is (= (:error v/number)
             (v/property-error :a nil v/number)))
      (is (= (:error v/number)
             (v/property-error :a :b v/number)))
      (is (= nil
             (v/property-error :a 3 v/number)))
      (is (= nil
             (v/property-error :a 3.14 v/number))))
    (testing "string"
        (is (= (:error v/string)
               (v/property-error :a nil v/string)))
        (is (= (:error v/string)
               (v/property-error :a :b  v/string)))
        (is (= nil
               (v/property-error :a "b" v/string))))
    (testing "timestamp"
      (is (= (:error v/timestamp)
             (v/property-error :a nil v/timestamp)))
      (is (= (:error v/timestamp)
             (v/property-error :a :b  v/timestamp)))
      (is (= (:error v/timestamp)
             (v/property-error :a "2013-05-01" v/timestamp)))
      (is (= nil
             (v/property-error :a #inst "2013-05-01" v/timestamp))))
    (testing "timestamp-string"
      (is (= (:error v/timestamp-string)
             (v/property-error :a nil v/timestamp-string)))
      (is (= (:error v/timestamp-string)
             (v/property-error :a :b v/timestamp-string)))
      (is (= (:error v/timestamp-string)
             (v/property-error :a #inst "2013-05-01" v/timestamp-string)))
      (is (= nil
             (v/property-error :a "2013-05-01" v/timestamp-string))))
    (testing "uuid"
      (is (= (:error v/uuid)
             (v/property-error :a nil v/uuid)))
      (is (= (:error v/uuid)
             (v/property-error :a :b  v/uuid)))
      (is (= (:error v/uuid)
             (v/property-error
               :a "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid)))
      (is (= nil
             (v/property-error
               :a #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid))))
    (testing "uuid-string"
      (is (= (:error v/uuid-string)
             (v/property-error :a nil v/uuid-string)))
      (is (= (:error v/uuid-string)
             (v/property-error :a :b v/uuid-string)))
      (is (= (:error v/uuid-string)
             (v/property-error
               :a #uuid "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid-string)))
      (is (= nil
             (v/property-error
               :a "d227317f-96aa-4e9b-a383-7e3a25a7712f" v/uuid-string))))))

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
        (is (= {}
               (v/errors {:a "b"} vs)))))))

(deftest valid?-test
  (testing "valid?"
    (testing "1 string"
      (let [vs {:a [v/string]}]
        (is (= false (v/valid? {:a :b} vs)))
        (is (= true  (v/valid? {} vs)))
        (is (= true  (v/valid? {:a "b"} vs)))))))
