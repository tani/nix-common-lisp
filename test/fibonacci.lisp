(defpackage #:fibonacci/test/fibonacci
  (:use #:cl #:rove #:fibonacci/src/fibonacci))
(in-package #:fibonacci/test/fibonacci)

(deftest fib-test
  (ok (= (fib 10) 55)))
