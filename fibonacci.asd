(defsystem #:fibonacci
  :build-operation program-op
  :entry-point "fibonacci:main"
  :components ((:file "src/fibonacci"))
  :in-order-to ((test-op (test-op #:fibonacci-test))))

(defsystem #:fibonacci/test
  :depends-on (#:fiveam #:fibonacci)
  :components ((:file "test/fibonacci"))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :fibonacci)))
