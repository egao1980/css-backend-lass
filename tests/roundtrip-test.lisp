(in-package #:css-backend-lass/tests)

;;; LASS -> model -> write-stylesheet, minified and pretty; the model is a
;;; plain css-protocol stylesheet that its transforms accept.

(defparameter *theme*
  '((:import (url "fonts.css"))
    (:let* ((primary "#007bff")
            (pad "8px"))
     (":root" :--primary #(primary))
     (body :margin 0 :padding #(pad) :font-size (ceil (* 0.85 "16px")))
     ((:or h1 h2) :line-height 1.2
      ((:parent :hover) :color (darken #(primary) 10%)))
     (:media (:max-width 400px)
      (body :padding (/ #(pad) 2)))
     (:keyframes pulse
      (from :opacity 1)
      (to :opacity (fade-out "#000" 0.5)))
     (button :border 1px solid (mix #(primary) "#fff") :color red !important))))

(defparameter *theme-mini*
  (concatenate 'string
               "@import url(\"fonts.css\");"
               ":root{--primary:#007bff}"
               "body{margin:0;padding:8px;font-size:14.00px}"
               "h1,h2{line-height:1.2}"
               "h1:hover,h2:hover{color:#0062CC}"
               "@media (max-width:400px){body{padding:4.00px}}"
               "@keyframes pulse{from{opacity:1}to{opacity:rgba(0, 0, 0, 0.5)}}"
               "button{border:1px solid #80BDFF;color:red!important}"))

(defparameter *theme-pretty*
  (lines "@import url(\"fonts.css\");"
         ""
         ":root {"
         "  --primary: #007bff;"
         "}"
         ""
         "body {"
         "  margin: 0;"
         "  padding: 8px;"
         "  font-size: 14.00px;"
         "}"
         ""
         "h1, h2 {"
         "  line-height: 1.2;"
         "}"
         ""
         "h1:hover, h2:hover {"
         "  color: #0062CC;"
         "}"
         ""
         "@media (max-width:400px) {"
         "  body {"
         "    padding: 4.00px;"
         "  }"
         "}"
         ""
         "@keyframes pulse {"
         "  from {"
         "    opacity: 1;"
         "  }"
         ""
         "  to {"
         "    opacity: rgba(0, 0, 0, 0.5);"
         "  }"
         "}"
         ""
         "button {"
         "  border: 1px solid #80BDFF;"
         "  color: red !important;"
         "}"))

(deftest roundtrip-minified-and-pretty
  (let ((sheet (compile-lass *theme*)))
    (ok (typep sheet 'stylesheet))
    (ok (equal *theme-mini* (write-stylesheet sheet :pretty nil)))
    (ok (equal *theme-pretty* (write-stylesheet sheet)))
    (ok (equal (write-stylesheet sheet) (write-stylesheet (compile-lass *theme*)))
        "byte-stable: the same input gives the same output")
    (ok (eq sheet (compile-stylesheet sheet)) "a compiled sheet is a css-protocol stylesheet")))

(deftest roundtrip-model-shape
  (let ((rules (stylesheet-rules (compile-lass *theme*))))
    (ok (equal '(import-rule rule rule rule rule media-rule keyframes-rule rule)
               (mapcar #'type-of rules)))
    (let ((hover (fifth rules)))
      (ok (equal '("h1:hover" "h2:hover")
                 (mapcar #'selector-text (rule-selectors hover))))
      (ok (equal "h1" (compound-type (first (selector-compounds (first (rule-selectors hover))))))
          "selectors are parsed structures")
      (ok (equal '(":hover")
                 (compound-others (first (selector-compounds (first (rule-selectors hover))))))))
    (ok (equal "(max-width:400px)" (media-rule-query (sixth rules))))
    (ok (custom-property-p (first (rule-declarations (second rules)))))))

(deftest roundtrip-transforms-accept-the-model
  (let ((sheet (compile-lass '((":root" :--x 1) (p :margin 0) (.card :padding 0)))))
    (ok (equal "#c{--x:1}#c p{margin:0}#c .card{padding:0}"
               (write-stylesheet (scope-stylesheet sheet "#c") :pretty nil)))
    (ok (equal ":root{--x:1}p{margin:0}"
               (write-stylesheet
                (used-rules sheet (html-protocol:make-html-element "p"))
                :pretty nil)))))
