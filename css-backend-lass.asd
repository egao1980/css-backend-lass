(defsystem "css-backend-lass"
  :version "0.1.0"
  :description "css-protocol backend: LASS S-expressions to the stylesheet model, with unit arithmetic, colours, :let*, :include and an ASDF stylesheet component"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "css-protocol" "0.1.0")
               (:version "lass" "0.6.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "backend")
               (:file "asdf"))
  :in-order-to ((test-op (test-op "css-backend-lass/tests"))))

(defsystem "css-backend-lass/tests"
  :depends-on ("css-backend-lass" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "compile-test")
               (:file "units-test")
               (:file "colour-test")
               (:file "let-include-test")
               (:file "condition-test")
               (:file "roundtrip-test")
               (:file "backend-test")
               (:file "asdf-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
