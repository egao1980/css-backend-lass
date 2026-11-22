;;;; css-backend-lass: LASS S-expressions -> the css-protocol stylesheet model.
;;;; Brief: cl-stack/docs/capabilities/css.md (H4h-0b-1).

(defpackage #:css-backend-lass
  (:use #:cl)
  (:import-from #:css-protocol
                #:css-error
                #:css-syntax-error
                #:skip-invalid)
  (:export
   ;; conditions (src/conditions.lisp)
   #:lass-syntax-error
   #:lass-syntax-error-cause
   #:lass-file-error
   #:lass-file-error-pathname
   #:lass-file-error-cause
   #:lass-include-cycle
   #:lass-include-chain

   ;; backend (src/backend.lisp)
   #:lass-backend
   #:*lass-backend*
   #:lass-source
   #:make-lass-source
   #:lass-source-forms
   #:lass-source-pathname
   #:lass-source-variables
   #:compile-lass
   #:compile-lass-file

   ;; ASDF component (src/asdf.lisp)
   #:lass-stylesheet
   #:lass-stylesheet-output
   #:lass-stylesheet-pretty))

;;; LASS files (COMPILE-LASS-FILE, :INCLUDE, the :LASS-STYLESHEET component)
;;; are read with *PACKAGE* bound to this package and *READ-EVAL* NIL, so a
;;; #(name) variable reference in one file is the same symbol in every other
;;; file. A string key in :VARIABLES is interned here (upcased).

(defpackage #:css-backend-lass-user
  (:use #:cl))
