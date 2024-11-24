(defsystem #:fibonacci
  :class :package-inferred-system
  :version "0.1.0"
  :build-operation program-op
  :entry-point "fibonacci:main"
  :depends-on (#:fibonacci/src/fibonacci)
  :in-order-to ((test-op (test-op #:fibonacci/test))))

(defsystem #:fibonacci/test
  :depends-on (#:fiveam #:fibonacci/test/fibonacci)
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :fibonacci)))
