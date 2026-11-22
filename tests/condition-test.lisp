(in-package #:css-backend-lass/tests)

;;; LASS errors and invalid input -> css-protocol conditions and restarts.
;;; Every HANDLER-BIND sits inside its OK: Rove's OK catches errors itself.

;; A compiled (:text ...) object, which the model does not hold.
(lass:define-special-block css1-test-raw (text)
  (list (list :text text)))

(deftest lass-errors-map-to-lass-syntax-error
  (let ((c (condition-of (lambda () (compile-lass '((x :a b) (a 5)))))))
    (ok (typep c 'lass-syntax-error))
    (ok (typep c 'css-syntax-error))
    (when c
      (ok (equal '(a 5) (css-error-input c)) "input: the top-level form")
      (ok (typep (lass-syntax-error-cause c) 'error))
      (ok (not (typep (lass-syntax-error-cause c) 'css-error)))))
  (let ((c (condition-of (lambda () (compile-lass '((nav (ul 5))))))))
    (ok (typep c 'lass-syntax-error))
    (when c (ok (equal '(nav (ul 5)) (css-error-input c)) "the top-level form, not the inner one")))
  (ok (typep (condition-of (lambda () (compile-lass '((:media))))) 'lass-syntax-error)
      "a malformed special block")
  (ok (typep (condition-of (lambda () (compile-lass '((a :color (darken "#fff")))))) 'lass-syntax-error)
      "a property function with the wrong argument count")
  (ok (typep (condition-of (lambda () (compile-lass '((:let* ((a)) (p :x #(a))))))) 'lass-syntax-error)
      "a malformed :let* binding"))

(deftest lass-syntax-error-restarts
  (ok (equal "x{a:b}y{c:d}"
             (handler-bind ((lass-syntax-error #'skip-invalid))
               (mini '((x :a b) (a 5) (y :c d)))))
      "skip-invalid drops the form")
  (ok (equal "x{a:b}b{color:red}"
             (handler-bind ((lass-syntax-error (lambda (c) (use-value '(b :color red) c))))
               (mini '((x :a b) (a 5)))))
      "use-value compiles a replacement form")
  (ok (equal "b{c:d}"
             (let ((n 0))
               (handler-bind ((lass-syntax-error
                                (lambda (c)
                                  (incf n)
                                  (use-value (if (= n 1) '(a 6) '(b :c d)) c))))
                 (mini '((a 5))))))
      "the replacement is compiled again (and may signal again)"))

(deftest model-conditions-pass-through
  (let ((c (condition-of (lambda () (compile-lass '((a :color "red;")))))))
    (ok (typep c 'css-invalid-value))
    (when c (ok (equal "red;" (css-error-input c)))))
  (ok (typep (condition-of (lambda () (compile-lass '((a :color))))) 'css-invalid-value)
      "a property with no value")
  (ok (typep (condition-of (lambda () (compile-lass '((|a{| :color red))))) 'css-invalid-selector))
  (ok (typep (condition-of (lambda () (compile-lass '((a :|col or| red))))) 'css-invalid-property))
  (ok (typep (condition-of (lambda () (compile-lass '((:media "print;" (a :x y)))))) 'css-invalid-prelude))
  (ok (typep (condition-of (lambda () (compile-lass '((:keyframes "" (from :x 0)))))) 'css-invalid-identifier))
  (ok (equal "a{margin:0}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((a :color "red;" :margin 0)))))
      "css-protocol's skip-invalid drops the declaration")
  (ok (equal "b{c:d}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((a :color "red;") (b :c d)))))
      "a rule left with no declarations is dropped")
  (ok (equal "b{color:red}"
             (handler-bind ((css-invalid-selector (lambda (c) (use-value "b" c))))
               (mini '((|a{| :color red))))))
  (ok (equal "a{color:blue}"
             (handler-bind ((css-invalid-value (lambda (c) (use-value "blue" c))))
               (mini '((a :color "red;")))))))

(deftest no-end-tag-can-come-out
  (ok (typep (condition-of (lambda () (compile-lass '((a :content "</style>"))))) 'css-invalid-value))
  (ok (typep (condition-of (lambda () (compile-lass '((a :x "</p>"))))) 'css-invalid-value))
  (ok (typep (condition-of (lambda () (compile-lass '(("a</b" :x y))))) 'css-invalid-selector)))

(deftest unbound-variable
  (let ((c (condition-of (lambda () (compile-lass '((a :color #(nope))))))))
    (ok (typep c 'css-invalid-value) "not the text NIL")
    (when c (ok (equal "nope" (css-error-input c)))))
  (ok (equal "a{color:red}"
             (handler-bind ((css-invalid-value (lambda (c) (use-value "red" c))))
               (mini '((a :color #(nope)))))))
  (ok (equal "a{margin:0}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((a :color #(nope) :margin 0)))))))

(deftest function-error-restarts
  (ok (equal "a{margin:0}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((a :color (darken "red" 10%) :margin 0)))))
      "skip-invalid drops the declaration holding the call")
  (ok (equal "a{color:#000;margin:0}"
             (handler-bind ((css-invalid-value (lambda (c) (use-value "#000" c))))
               (mini '((a :color (darken "red" 10%) :margin 0)))))
      "use-value: the text of the call")
  (ok (equal "a{border:1px solid;margin:0}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((a :border 1px solid :color (fade-out "x" 1) :margin 0)))))
      "only that declaration")
  (ok (equal "a{margin:0}b{x:y}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((:let* ((c (darken "red" 10%)))
                         (a :color #(c) :margin 0)
                         (b :x y))))))
      "through a :let* binding, every declaration using it is dropped")
  (ok (equal "a{width:2.00px}"
             (handler-bind ((css-invalid-value (lambda (c) (use-value "2.00px" c))))
               (mini '((a :width (+ "1em" "2px"))))))
      "use-value replaces the whole call, though the input is the bad operand")
  (ok (equal "a{width:calc(1em + 2px)}"
             (handler-bind ((css-invalid-value (lambda (c) (use-value "calc(1em + 2px)" c))))
               (mini '((a :width (* 2 (+ "1em" "2px")))))))
      "the inner call is replaced; the outer * then fails on that text and is replaced too"))

(deftest text-objects
  (let ((c (condition-of (lambda () (compile-lass '((:css1-test-raw "a{}")))))))
    (ok (typep c 'css-invalid-value))
    (when c (ok (equal "a{}" (css-error-input c)))))
  (ok (equal "b{c:d}"
             (handler-bind ((css-invalid-value #'skip-invalid))
               (mini '((:css1-test-raw "a{}") (b :c d))))))
  (ok (equal "x{a:b}"
             (handler-bind ((css-invalid-value
                              (lambda (c)
                                (use-value (make-rule "x" (list (make-declaration "a" "b"))) c))))
               (mini '((:css1-test-raw "a{}"))))))
  (ok (signals (handler-bind ((css-invalid-value (lambda (c) (use-value 42 c))))
                 (compile-lass '((:css1-test-raw "a{}"))))
               'type-error)
      "use-value takes css-nodes only"))

(deftest-parametrize unsupported-at-rules
    ((form input)
     ('(:page :first :margin 1in) "@page :first")
     ('(:charset "utf-8") "@charset utf-8")
     ('(:namespace svg "http://www.w3.org/2000/svg") "@namespace svg http://www.w3.org/2000/svg"))
  (let ((c (condition-of (lambda () (compile-lass (list form))))))
    (ok (typep c 'css-invalid-prelude) (format nil "~S" form))
    (ok (and c (equal input (css-error-input c))) (format nil "input of ~S" form))))

(deftest unsupported-at-rule-restarts
  (ok (typep (condition-of (lambda () (compile-lass '((:document (:url-prefix "http://x") (a :x y))))))
             'css-invalid-prelude)
      "@document")
  (ok (equal "a{x:y}"
             (handler-bind ((css-invalid-prelude #'skip-invalid))
               (mini '((:charset "utf-8") (:page :first :margin 1in) (a :x y))))))
  (ok (equal "@layer a;b{c:d}"
             (handler-bind ((css-invalid-prelude
                              (lambda (c)
                                (use-value (list (make-layer-rule "a")
                                                 (make-rule "b" (list (make-declaration "c" "d"))))
                                           c))))
               (mini '((:page :first :margin 1in)))))
      "use-value: a list of nodes in place"))

(deftest backend-conditions-are-css-errors
  (ok (subtypep 'lass-syntax-error 'css-syntax-error))
  (ok (subtypep 'lass-file-error 'css-syntax-error))
  (ok (subtypep 'lass-include-cycle 'lass-file-error))
  (ok (subtypep 'lass-syntax-error 'css-error)))
