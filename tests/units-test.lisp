(in-package #:css-backend-lass/tests)

;;; classie's unit arithmetic, ceil / ceiling and values.

(deftest classie-readme-examples
  ;; README.org of egao1980/classie, through the model instead of LASS's writer.
  (ok (equal "a{background:14.00px}" (mini '((a :background (ceil (* 0.85 "16px")))))))
  (ok (equal "a{height:14.00px}" (mini '((a :height (ceil (* 0.85 "16px")))))))
  (ok (equal "div{width:9.08in}" (mini '((div :width (+ 2in (ceil (* 85% "800px"))))))))
  (ok (equal "div{background:#CCCCCC}" (mini '((div :background (darken "#fff" 20%))))))
  (ok (equal (lines "a {" "  background: 14.00px;" "}")
             (pretty '((a :background (ceil (* 0.85 "16px"))))))))

(deftest-parametrize arithmetic
    ((form expected)
     ;; lengths
     ('(* 0.85 "16px") "13.60px")
     ('(* 85% "800px") "680.00px")
     ('(* "800px" 85%) "680.00px")
     ('(+ 1px 2px) "3.00px")
     ('(+ "10px" "2pt") "12.67px")
     ('(- 1in "48px") "0.50in")
     ('(+ "1pt" "1px") "1.75pt")
     ('(* 3 "1.5pt") "4.50pt")
     ('(/ "800px" 2) "400.00px")
     ('(- "10px" "20px") "-10.00px")
     ('(+ 1 "2px") "3.00px")
     ('(/ "1px" 8) "0.13px")
     ('(/ "-1px" 8) "-0.13px")
     ;; ratios of like quantities are unitless
     ('(/ "800px" "2px") "400")
     ('(/ "1in" "48px") "2")
     ('(/ 50% 25%) "2")
     ;; percents
     ('(* 50% 50%) "25.00%")
     ('(+ 10% 20%) "30.00%")
     ('(- 10% 20%) "-10.00%")
     ('(/ 50% 2) "25.00%")
     ('(+ 5 10%) "15.00%")
     ('(/ "100px" 50%) "200.00px")
     ;; unitless
     ('(* 2 3) "6")
     ('(+ 1 2) "3")
     ('(/ 1 3) "0.33")
     ('(* 0.5 3) "1.50")
     ;; nesting and variables
     ('(+ 2in (* 85% "800px")) "9.08in")
     ('(* 2 (+ "1px" "1px")) "4.00px"))
  (ok (equal expected (value-of form)) (format nil "~S" form)))

(deftest-parametrize ceil-and-ceiling
    ((form expected)
     ('(ceil 49.5%) "50.00%")
     ('(ceiling 49.5%) "100.00%")
     ('(ceil 40%) "40.00%")
     ('(ceiling 40%) "100.00%")
     ('(ceiling 0%) "0.00%")
     ('(ceiling 100.5%) "200.00%")
     ('(ceil "13.6px") "14.00px")
     ('(ceiling "13.6px") "14.00px")
     ('(ceil "0.5in") "1.00in")
     ('(ceil "-1.5px") "-1.00px")
     ('(ceil 1.2) "2")
     ('(ceiling 1.2) "2")
     ('(ceil 3) "3"))
  (ok (equal expected (value-of form)) (format nil "~S" form)))

(deftest engels-font-size-small
  ;; engels.lass: (font-size-small (ceiling (* #(font-size) 0.85)))
  (ok (equal "sup{font-size:14.00px}"
             (mini '((:let* ((font-size "16px")
                             (font-size-small (ceiling (* #(font-size) 0.85))))
                       (sup :font-size #(font-size-small))))))))

(deftest values-function
  (ok (equal "1px solid #ccc" (value-of '(values 1px solid "#ccc"))))
  (ok (equal "hr{border-top:1px solid rgba(0, 0, 0, 0.12)}"
             (mini '((:let* ((dividers "rgba(0, 0, 0, 0.12)"))
                       (hr :border-top (values 1px solid #(dividers))))))))
  (ok (equal "0 0 0 0.2rem 8.00px" (value-of '(values 0 0 0 0.2rem (* 2 "4px"))))
      "values resolves property-function arguments"))

(deftest-parametrize arithmetic-errors
    ((form input)
     ('(+ "1em" "2px") "1em")
     ('(* "2rem" 2) "2rem")
     ('(+ "red" 1) "red")
     ('(ceil "abc") "abc")
     ('(ceiling "1e3px") "1e3px")
     ('(/ "1px" 0) "0"))
  (let ((c (condition-of (lambda () (value-of form)))))
    (ok (typep c 'css-invalid-value) (format nil "~S" form))
    (ok (and c (equal input (css-error-input c))) (format nil "input of ~S" form))))

(deftest-parametrize incompatible-units
    ((form)
     ('(+ 10% "1px"))
     ('(- "1px" 10%))
     ('(* "2px" "3px"))
     ('(/ 1 "2px"))
     ('(/ 2 50%))
     ('(/ 50% "1px")))
  (ok (typep (condition-of (lambda () (value-of form))) 'css-invalid-value)
      (format nil "~S" form)))
