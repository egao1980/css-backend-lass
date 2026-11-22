;;;; Special blocks added to LASS (:let* :include :layer :scope), the gated
;;;; unbound-variable check, and LASS file reading.

(in-package #:css-backend-lass)

;;; #(name) with no binding: CSS-INVALID-VALUE inside backend compiles.
;;; Strings are not SIMPLE-VECTORs, so literals never reach this method.

(defmethod lass:resolve :around ((thing simple-vector))
  (if (and *in-backend* (plusp (length thing)))
      (multiple-value-bind (value found) (gethash (aref thing 0) lass:*vars*)
        (declare (ignore value))
        (if found
            (call-next-method)
            (%signal-invalid-value (string-downcase (princ-to-string (aref thing 0))))))
      (call-next-method)))

;;; (:let* ((name value-form)...) body...)

(defun %copy-vars ()
  (let ((table (make-hash-table)))
    (maphash (lambda (k v) (setf (gethash k table) v)) lass:*vars*)
    table))

(lass:define-special-block let* (bindings &rest body)
  (unless (listp bindings)
    (error "~S: the :let* bindings must be a list" bindings))
  (let ((lass:*vars* (%copy-vars)))
    (dolist (binding bindings)
      (unless (and (consp binding)
                   (symbolp (first binding))
                   (consp (rest binding))
                   (null (cddr binding)))
        (error "~S is not a :let* binding (name value-form)" binding))
      (setf (gethash (first binding) lass:*vars*) (lass:resolve (second binding))))
    (apply #'lass:compile-sheet body)))

;;; LASS files

(defun %read-forms (pathname)
  "Every top-level form of the LASS file PATHNAME -> (values FORMS NIL), or
\(values NIL CONDITION) when it cannot be opened or read."
  (handler-case
      (with-open-file (in pathname :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*package* (find-package '#:css-backend-lass-user))
                (*read-eval* nil)
                (eof (list nil)))
            (values (loop for form = (read in nil eof)
                          until (eq form eof)
                          collect form)
                    nil))))
    (error (e) (values nil e))))

(defun %file-on-stack-p (truename)
  (and truename
       (member (namestring truename) *include-stack*
               :key #'namestring :test #'string=)))

(defun %read-lass-file (designator base)
  "Read the LASS file DESIGNATOR (merged with BASE). -> (values FORMS FILE),
FILE being its truename. LASS-INCLUDE-CYCLE when the file is already being
compiled, LASS-FILE-ERROR when it cannot be read; their USE-VALUE takes
another designator (same BASE), SKIP-INVALID returns (values NIL NIL)."
  (loop
    (let* ((path (merge-pathnames designator base))
           (truename (probe-file path))
           (problem
             (if (%file-on-stack-p truename)
                 (make-condition 'lass-include-cycle
                                 :input designator :pathname path
                                 :chain (append *include-stack* (list truename)))
                 (multiple-value-bind (forms cause) (%read-forms path)
                   (if cause
                       (make-condition 'lass-file-error
                                       :input designator :pathname path :cause cause)
                       (return (values forms (or truename path))))))))
      (restart-case (error problem)
        (use-value (new-designator)
          :report "Read another LASS file (resolved against the same base)."
          :interactive (lambda ()
                         (format *query-io* "~&LASS file: ")
                         (force-output *query-io*)
                         (list (read-line *query-io*)))
          (setf designator new-designator))
        (skip-invalid ()
          :report "Skip this LASS file (it contributes nothing)."
          (return (values nil nil)))))))

(defun %compile-file-forms (forms file)
  "Compile FORMS of FILE (each through the per-form wrapper) with FILE as
the include base and on the include stack -> compiled LASS items."
  (let ((*include-base* file)
        (*include-stack* (append *include-stack* (list file))))
    (loop for form in forms
          append (%compile-form form))))

;;; (:include "file.lass")

(lass:define-special-block include (designator)
  (multiple-value-bind (forms file)
      (%read-lass-file designator (or *include-base* *default-pathname-defaults*))
    (when file
      (%compile-file-forms forms file))))

;;; (:layer names body...) -> (:superblock "layer" (:selector literal...) . blocks)
;;; No body: a statement (no blocks). A body that compiles to nothing gives
;;; nothing, so "no blocks" always means a statement.

(defun %literal (text)
  (list :constraint :literal text))

(defun %selector-text (constraints)
  "LASS's minified text for a :selector constraint list."
  (with-output-to-string (s)
    (let ((lass:*pretty* nil))
      (lass:write-sheet-object :selector constraints s))))

(lass:define-special-block layer (names &rest body)
  (let ((selector (when names
                    (list* :selector
                           (mapcar (lambda (name) (%literal (lass:resolve name)))
                                   (if (listp names) names (list names)))))))
    (if body
        (let ((blocks (apply #'lass:compile-sheet body)))
          (when blocks
            (list (list* :superblock "layer" selector blocks))))
        (list (list :superblock "layer" selector)))))

;;; (:scope start body...) / (:scope (:to start end) body...)
;;; -> (:superblock "scope" (:selector start-literal [end-literal]) . blocks)

(lass:define-special-block scope (start &rest body)
  (multiple-value-bind (from to)
      (if (and (consp start) (eq (first start) :to))
          (destructuring-bind (from to) (rest start) (values from to))
          (values start nil))
    (let ((blocks (apply #'lass:compile-sheet body)))
      (when blocks
        (list (list* :superblock "scope"
                     (list* :selector
                            (%literal (%selector-text (lass:compile-selector from)))
                            (when to
                              (list (%literal (%selector-text (lass:compile-selector to))))))
                     blocks))))))

;;; :box-shadow inside backend compiles: one unprefixed declaration with the
;;; values space-joined (LASS's default for a property), not LASS's five
;;; vendor-prefixed copies of a fixed (color x y blur spread inset) list --
;;; box-shadow has needed no prefix for a decade and takes any value list
;;; (tests: colours-in-context). Plain LASS:COMPILE-SHEET is unchanged.

(defmethod lass:compile-property :around ((key (eql :box-shadow)) value)
  (if *in-backend*
      (list (lass:make-property
             "box-shadow"
             (format nil "~{~A~^ ~}"
                     (mapcar #'lass:resolve (if (listp value) value (list value))))))
      (call-next-method)))
