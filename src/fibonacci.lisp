(defpackage #:fibonacci/src/fibonacci
  (:nicknames #:fibonacci)
  (:use #:cl)
  (:export #:main #:fib))
(in-package #:fibonacci/src/fibonacci)

(defun fib (n)
  (if (<= n 2)
    1
    (+ (fib (- n 2)) (fib (- n 1)))))

(defun main ()
  (format t "fib(10) = ~a~%" (fib 10)))
