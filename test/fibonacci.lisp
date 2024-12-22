(defpackage #:fibonacci/test/fibonacci
  (:use #:cl #:fiveam #:fibonacci))
(in-package #:fibonacci/test/fibonacci)

(def-suite :fibonacci)
(in-suite :fibonacci)

(test fib-test
  (is (= (fib 10) 55)))
