(defsystem #:fibonacci
  :class :package-inferred-system
  :depends-on (#:fibonacci/src/fibonacci)
  :in-order-to ((test-op (load-op #:fibonacci/test)))
  :perform (test-op (o c) (symbol-call :rove :run c)))

(defsystem #:fibonacci/test :depends-on (#:rove))
