(defpackage #:css-backend-lass/tests
  (:use #:cl #:rove #:css-protocol #:css-backend-lass))

(in-package #:css-backend-lass/tests)

(defun lines (&rest lines)
  "Each line followed by a newline: the pretty golden format."
  (format nil "~{~A~%~}" lines))

(defun mini (forms &rest keys)
  "FORMS compiled with COMPILE-LASS, written minified."
  (write-stylesheet (apply #'compile-lass forms keys) :pretty nil))

(defun pretty (forms &rest keys)
  "FORMS compiled with COMPILE-LASS, written pretty."
  (write-stylesheet (apply #'compile-lass forms keys)))

(defun fixture (name)
  "Pathname of tests/fixtures/NAME."
  (asdf:system-relative-pathname "css-backend-lass/tests"
                                 (concatenate 'string "tests/fixtures/" name)))

(defun mini-file (name &rest keys)
  (write-stylesheet (apply #'compile-lass-file (fixture name) keys) :pretty nil))

(defun value-of (form)
  "The value text of the single declaration (x :p FORM)."
  (let* ((sheet (compile-lass `((x :p ,form))))
         (rule (first (stylesheet-rules sheet))))
    (declaration-value (first (rule-declarations rule)))))

(defun condition-of (thunk)
  "The CSS-ERROR THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (css-error (c) c)))

(defun names (pathnames)
  (mapcar #'file-namestring pathnames))
