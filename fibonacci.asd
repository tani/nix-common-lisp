(defsystem #:fibonacci
  :class :package-inferred-system
  :depends-on (#:fibonacci/src/fibonacci)
  :in-order-to ((test-op (test-op #:fibonacci/test))))

(defsystem #:fibonacci/test
  :depends-on (#:parachute #:fibonacci/test/fibonacci)
  :perform (test-op (o c)
             (symbol-call :parachute :test :fibonacci/test/fibonacci)))
