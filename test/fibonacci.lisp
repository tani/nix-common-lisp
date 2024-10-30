(defpackage #:fibonacci/test/fibonacci
  (:use #:cl #:parachute #:fibonacci/src/fibonacci))
(in-package #:fibonacci/test/fibonacci)

(define-test fib-test
  (is = (fib 10) 55))
