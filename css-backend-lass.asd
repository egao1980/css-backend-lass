(defsystem "css-backend-lass"
  :version "0.1.0"
  :description "css-protocol backend: LASS S-expressions to the stylesheet model, with unit arithmetic, colours, :let*, :include and an ASDF stylesheet component"
  :author "egao1980"
  :license "MIT"
  :depends-on ("lass")
  :serial t
  :pathname "src"
  :components ((:file "package")))
