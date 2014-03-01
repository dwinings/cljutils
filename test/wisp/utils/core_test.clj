(ns wisp.utils.core_test
  (:use clojure.test
        wisp.utils.core))

(deftest vararg-with-string
  (let [orig (into-array String ["hello"])
        varg (vararg String "hello")]
    (do
      (is (= (type orig) (type varg)))
      (is (= (count orig) (count varg))))))

(deftest vararg-with-list
  (let [orig (into-array String ["hello"])
        varg (vararg String '("hello"))]
    (do
      (is (= (type orig) (type varg)))
      (is (= (count orig) (count varg))))))

(deftest vararg-with-multiple-args
  (let [orig (into-array String ["hello" "goodbye"])
        varg (vararg String "hello" "goodbye")]
    (do
      (is (= (type orig) (type varg)))
      (is (= (count orig) (count varg))))))

(deftest vararg-with-vector
  (let [orig (into-array String ["hello"])
        varg (vararg String ["hello"])]
    (do
      (is (= (type orig) (type varg)))
      (is (= (count orig) (count varg))))))
