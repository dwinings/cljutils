(ns wisp.utils.core_test
  (:use clojure.test
        wisp.utils.core)
  (:require [clojure.java.io :as io])
  (:import (java.io StringReader BufferedReader)))

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


(deftest wc-test
  ;; Mocking is actually pretty easy.
  (with-redefs  [io/reader (fn [& rest]
                             (-> "this is my\ntest line\nof yore"
                                 (StringReader.)
                                 (BufferedReader.)))
                 get-attrs (fn [thing] {"size" 34})]
    (let [result (wc "")]
      (do
        (is (= (:lines result) 3))
        (is (= (:chars result) 34))
        (is (= (:words result) 7))))))

