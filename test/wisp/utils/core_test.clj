(ns wisp.utils.core_test
  (:use clojure.test
        wisp.utils.core)
  (:require [clojure.java.io :as io])
  (:import (java.io StringReader StringWriter BufferedReader BufferedWriter)))

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

(deftest tee-test
  (let [out-buf (atom (StringWriter.))
        test-str "hello I am the test"
        test-seq '("hello" "I" "am" "the" "test")
        test-vec ["hello" "I" "am" "the" "test"]]
    (with-redefs [io/writer (fn [& rest] (BufferedWriter. @out-buf))]
      (do
        ;; Test tee with a string.
        (tee test-str "") 
        (is (= (.toString @out-buf) (str test-str "\n")))
        (swap! out-buf (fn [& rest] (StringWriter.)))

        ;; Test tee with a list.
        (tee test-seq "")
        (is (= (.toString @out-buf)
               (reduce #(str %1 %2 "\n") "" test-seq)))
        (swap! out-buf (fn [& rest] (StringWriter.)))

        ;; Test tee with a vector.
        (tee test-vec "")
        (is (= (.toString @out-buf)
               (reduce #(str %1 %2 "\n") "" test-vec)))
        (swap! out-buf (fn [& rest] (StringWriter.)))))))

