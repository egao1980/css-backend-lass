(in-package #:css-backend-lass/tests)

;;; LASS constructs -> golden CSS through the model (minified and pretty).

(deftest plain-block
  (ok (equal "a{color:red}" (mini '((a :color red)))))
  (ok (equal (lines "a {" "  color: red;" "  margin: 0 auto;" "}")
             (pretty '((a :color red :margin 0 auto)))))
  (ok (equal "" (mini '())) "no forms: an empty sheet"))

(deftest nesting
  (ok (equal "nav{margin:0}nav ul{list-style:none}nav ul li{display:inline-block}"
             (mini '((nav :margin 0 (ul :list-style none (li :display inline-block)))))))
  (ok (equal (lines "nav {" "  margin: 0;" "}"
                    ""
                    "nav ul {" "  list-style: none;" "}")
             (pretty '((nav :margin 0 (ul :list-style none))))))
  (ok (equal "nav ul{list-style:none}" (mini '((nav (ul :list-style none)))))
      "a block with no properties writes nothing (LASS parity)")
  (ok (equal "" (mini '((a)))) "an empty block writes nothing"))

(deftest parent-selector
  (ok (equal (concatenate 'string
                          "article,section{background:black}"
                          "article.bright,section.bright{background:white}"
                          "article:hover,section:hover{color:red}")
             (mini '(((:or article section)
                      :background black
                      ((:parent .bright) :background white)
                      ((:parent ":hover") :color red))))))
  (ok (equal (lines "article, section {" "  background: black;" "}"
                    ""
                    "article.bright, section.bright {" "  background: white;" "}")
             (pretty '(((:or article section)
                        :background black
                        ((:parent .bright) :background white)))))))

(deftest selector-combinations
  (ok (equal "input[type=\"text\"]:focus,input[type=\"email\"]:focus{color:red}"
             (mini '(((:and input (:= type (:or text email)) :focus) :color red)))))
  (ok (equal "a:not(.b){c:d}" (mini '(((:and a (:not .b)) :c d)))))
  (ok (equal "ul>li{margin:0}" (mini '(("ul  >  li" :margin 0))))
      "a literal selector is canonicalised by css-protocol")
  (ok (equal (lines "ul > li {" "  margin: 0;" "}") (pretty '(("ul > li" :margin 0)))))
  (ok (equal ":root{--bg:#fff}a{color:var(--bg)}"
             (mini '((":root" :--bg "#fff") (a :color (var --bg)))))
      "custom properties and var()"))

(deftest values-and-important
  (ok (equal "a{color:red!important;margin:0}"
             (mini '((a :color red !important :margin 0)))))
  (ok (equal (lines "a {" "  color: red !important;" "}")
             (pretty '((a :color red !important)))))
  (let ((d (first (rule-declarations (first (stylesheet-rules
                                             (compile-lass '((a :color red !important)))))))))
    (ok (equal "red" (declaration-value d)))
    (ok (declaration-important-p d)))
  (ok (equal "a{line-height:1.5;z-index:10}" (mini '((a :line-height 1.5 :z-index 10)))))
  (ok (equal "a{content:\"x\"}" (mini '((a :content "x")))))
  (ok (equal "a{border:1px solid rgba(0, 0, 0, 0.12)}"
             (mini '((a :border 1px solid "rgba(0, 0, 0, 0.12)"))))))

(deftest lass-property-functions-pass-through
  (ok (equal (concatenate 'string
                          "a{-moz-user-select:none;-o-user-select:none;"
                          "-webkit-user-select:none;-ms-user-select:none;user-select:none}")
             (mini '((a :user-select none))))
      "LASS's browser properties")
  (ok (equal "a{background:url(\"x.png\")}" (mini '((a :background (url "x.png"))))))
  (ok (equal "a{color:rgb(1,2,3)}" (mini '((a :color (rgb 1 2 3)))))))

(deftest calc-keeps-lass-output
  (ok (equal "a{width:calc((100%) - (2px))}" (mini '((a :width (calc (- 100% 2px)))))))
  (ok (equal "a{width:calc((1em) + ((2) * (3px)))}"
             (mini '((a :width (calc (+ 1em (* 2 3px)))))))
      "arithmetic inside calc is calc's, not evaluated"))

(deftest media
  (ok (equal "@media screen and (max-width:400px){a{color:red}b{color:blue}}"
             (mini '((:media (:and screen (:max-width 400px)) (a :color red) (b :color blue))))))
  (ok (equal (lines "@media print {" "  a {" "    color: black;" "  }" "}")
             (pretty '((:media "print" (a :color black))))))
  (ok (equal "@media (max-width: 400px){nav ul{margin:0}}"
             (mini '((:media "(max-width: 400px)" (nav (ul :margin 0)))))))
  (ok (equal "a{x:y}" (mini '((:media print (b)) (a :x y))))
      "a media rule left empty is dropped"))

(deftest supports
  (ok (equal "@supports (display: grid){a{display:grid}}"
             (mini '((:supports "(display: grid)" (a :display grid)))))))

(deftest keyframes
  (ok (equal "@keyframes fade{from{opacity:0}50%,to{opacity:1}}"
             (mini '((:keyframes fade (from :opacity 0) ((:or 50% to) :opacity 1))))))
  (ok (equal (lines "@keyframes fade {" "  from {" "    opacity: 0;" "  }"
                    ""
                    "  to {" "    opacity: 1;" "  }" "}")
             (pretty '((:keyframes fade (from :opacity 0) (to :opacity 1))))))
  (let ((rule (first (stylesheet-rules
                      (compile-lass '((:keyframes spin (from :x 0) ((:or 50% to) :x 1))))))))
    (ok (typep rule 'keyframes-rule))
    (ok (equal "spin" (keyframes-rule-name rule)))
    (ok (equal '(("from") ("50%" "to"))
               (mapcar #'keyframe-selectors (keyframes-rule-keyframes rule))))))

(deftest font-face
  (ok (equal "@font-face{font-family:Inter;src:url(\"inter.woff2\")}"
             (mini '((:font-face :font-family "Inter" :src (url "inter.woff2"))))))
  (ok (typep (first (stylesheet-rules (compile-lass '((:font-face :font-family "Inter")))))
             'font-face-rule)))

(deftest import
  (ok (equal "@import url(\"a.css\") screen;@import url(\"b.css\");@import url(\"c.css\");a{color:red}"
             (mini '((:import (url "a.css") screen)
                     (:import "b.css")
                     (:import "\"c.css\"")
                     (a :color red)))))
  (let ((rule (first (stylesheet-rules (compile-lass '((:import (url "a.css") screen)))))))
    (ok (typep rule 'import-rule))
    (ok (equal "a.css" (import-rule-url rule)))
    (ok (equal "screen" (import-rule-media rule)))))

(deftest layer
  (ok (equal "@layer base,theme;" (mini '((:layer (base theme))))) "statement")
  (ok (equal (lines "@layer base, theme;") (pretty '((:layer (base theme))))))
  (ok (equal "@layer reset;" (mini '((:layer reset)))))
  (ok (equal "@layer base{a{color:red}}" (mini '((:layer base (a :color red))))))
  (ok (equal "@layer base.reset{a{margin:0}}" (mini '((:layer "base.reset" (a :margin 0))))))
  (ok (equal "@layer{a{color:red}}" (mini '((:layer nil (a :color red))))) "anonymous")
  (ok (equal "@layer x{@media print{a{color:black}}}"
             (mini '((:layer x (:media print (a :color black))))))
      "groups nest")
  (let ((rule (first (stylesheet-rules (compile-lass '((:layer (a b))))))))
    (ok (typep rule 'layer-rule))
    (ok (layer-rule-statement-p rule))
    (ok (equal '("a" "b") (layer-rule-names rule)))))

(deftest scope
  (ok (equal "@scope (.card){p{margin:0}}" (mini '((:scope .card (p :margin 0))))))
  (ok (equal "@scope (.card) to (.content){p{margin:0}}"
             (mini '((:scope (:to .card .content) (p :margin 0))))))
  (ok (equal (lines "@scope (.card) {" "  p {" "    margin: 0;" "  }" "}")
             (pretty '((:scope .card (p :margin 0))))))
  (ok (typep (first (stylesheet-rules (compile-lass '((:scope .card (p :margin 0))))))
             'scope-rule)))

(deftest lass-let-still-works
  (ok (equal "a{color:red}" (mini '((:let ((c "red")) (a :color #(c))))))))

(deftest order-is-kept
  (ok (equal "b{x:1}a{x:2}a b{x:3}c{x:4}"
             (mini '((b :x 1) (a :x 2 (b :x 3)) (c :x 4))))))
