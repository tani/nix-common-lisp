(defsystem #:fibonacci
  :build-operation program-op
  :build-pathname #.(or (uiop:getenv "CL_BUILD_PATHNAME") "fibonacci")
  :entry-point "fibonacci:main"
  :components ((:file "src/fibonacci"))
  :in-order-to ((test-op (test-op #:fibonacci-test))))

(defsystem #:fibonacci-test
  :depends-on (#:fiveam #:fibonacci)
  :components ((:file "test/fibonacci"))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :fibonacci)))
