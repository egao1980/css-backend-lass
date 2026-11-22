;;;; Property functions added to LASS's (global) property-function table:
;;;; classie's + - * / ceil ceiling values darken lighten fade-in fade-out
;;;; mix, and calc re-registered so its output stays LASS 0.6.0's.
;;;;
;;;; Every argument is LASS:RESOLVEd to text first (WITH-BACKEND-CALL); a
;;;; malformed argument count is a plain CL error from the lambda list, which
;;;; the per-form wrapper turns into LASS-SYNTAX-ERROR.

(in-package #:css-backend-lass)

(defmacro define-arithmetic (name op)
  `(lass:define-property-function ,name (a b &rest more)
     (with-backend-call ((ta a) (tb b))
       (let ((texts (list* ta tb (mapcar #'lass:resolve more))))
         (if (some #'%skipped-p texts)
             (%skipped-text)
             (%fold-arithmetic ,op texts))))))

(define-arithmetic + :+)
(define-arithmetic - :-)
(define-arithmetic * :*)
(define-arithmetic / :/)

(lass:define-property-function ceil (x)
  (with-backend-call ((tx x))
    (%ceil-text tx nil)))

(lass:define-property-function ceiling (x)
  (with-backend-call ((tx x))
    (%ceil-text tx t)))

(lass:define-property-function values (&rest args)
  (format nil "~{~A~^ ~}" (mapcar #'lass:resolve args)))

(lass:define-property-function darken (colour amount)
  (with-backend-call ((tc colour) (ta amount))
    (%colour-text (%adjust-lightness (%colour tc) (- (%fraction ta))))))

(lass:define-property-function lighten (colour amount)
  (with-backend-call ((tc colour) (ta amount))
    (%colour-text (%adjust-lightness (%colour tc) (%fraction ta)))))

(lass:define-property-function fade-in (colour amount)
  (with-backend-call ((tc colour) (ta amount))
    (%rgba-text (%adjust-alpha (%colour tc) (%fraction ta)))))

(lass:define-property-function fade-out (colour amount)
  (with-backend-call ((tc colour) (ta amount))
    (%rgba-text (%adjust-alpha (%colour tc) (- (%fraction ta))))))

(lass:define-property-function mix (a b &optional (weight "50%"))
  (with-backend-call ((ta a) (tb b) (tw weight))
    (let* ((c1 (%colour ta))
           (c2 (%colour tb))
           (p (%fraction tw)))
      (unless (<= 0 p 1) (%fail tw))
      (%colour-text (%mix c1 c2 p)))))

;;; calc: LASS 0.6.0's body, except that the arithmetic heads registered
;;; above are calc operators again (0.6.0 had no + - * / functions, so its
;;; output is unchanged).

(defun %calc-operator-p (head)
  (and (symbolp head)
       (member (string head) '("+" "-" "*" "/") :test #'string=)))

(lass:define-property-function calc (func)
  (with-output-to-string (out)
    (labels ((proc (func)
               (write-string "(" out)
               (if (or (not (listp func))
                       (and (lass:property-function (first func))
                            (not (%calc-operator-p (first func)))))
                   (write-string (lass:resolve func) out)
                   (destructuring-bind (func first &rest rest) func
                     (proc first)
                     (loop :for arg :in rest
                           :do (format out " ~A " (lass:resolve func))
                               (proc arg))))
               (write-string ")" out)))
      (write-string "calc" out)
      (proc func))))
