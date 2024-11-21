(defpackage #:fibonacci-test
  (:use #:cl #:fiveam #:fibonacci))
(in-package #:fibonacci-test)

(def-suite :fibonacci)
(in-suite :fibonacci)

(test fib-test
  (is (fib 10) 55))
