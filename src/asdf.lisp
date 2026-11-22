;;;; ASDF component :LASS-STYLESHEET. Class, initargs and OUTPUT-FILES GIVEN;
;;;; PERFORM COMPILE-OP is a STUB.
;;;;
;;;;   (defsystem "engels"
;;;;     :defsystem-depends-on ("css-backend-lass")
;;;;     :components ((:lass-stylesheet "engels" :output "../css/engels.css"
;;;;                                             :pretty nil)))
;;;;
;;;; The source is NAME.lass. COMPILE-OP compiles it with COMPILE-LASS-FILE
;;;; (includes relative to it) and writes (WRITE-STYLESHEET sheet :pretty P)
;;;; to the output: UTF-8, LF newlines on every OS, no extra trailing newline,
;;;; superseding an existing file, creating missing directories. :PRETTY
;;;; defaults to NIL (minified, like LASS:GENERATE). :OUTPUT is merged with
;;;; the component's pathname typed "css" (as LASS's LASS-FILE); without it
;;;; the output is NAME.css next to the source. LOAD-OP does nothing.
;;;;
;;;; As LASS does for LASS-FILE, the class is also defined in ASDF/INTERFACE so
;;;; the keyword :LASS-STYLESHEET resolves when this system is only in
;;;; :DEFSYSTEM-DEPENDS-ON.

(in-package #:css-backend-lass)

(defclass lass-stylesheet (asdf:source-file)
  ((output :initarg :output :initform nil :reader lass-stylesheet-output)
   (pretty :initarg :pretty :initform nil :reader lass-stylesheet-pretty))
  (:default-initargs :type "lass")
  (:documentation "ASDF component: compile a LASS file to a .css file through
the css-protocol model at COMPILE-OP."))

(defclass asdf/interface::lass-stylesheet (lass-stylesheet) ())

(defmethod asdf:source-file-type ((c lass-stylesheet) (s asdf:module)) "lass")

(defmethod asdf:output-files ((op asdf:compile-op) (c lass-stylesheet))
  (values
   (list (merge-pathnames
          (or (lass-stylesheet-output c)
              (pathname-name (asdf:component-pathname c)))
          (make-pathname :type "css" :defaults (asdf:component-pathname c))))
   t))

(defmethod asdf:perform ((op asdf:load-op) (c lass-stylesheet))
  t)

(defmethod asdf:perform ((op asdf:compile-op) (c lass-stylesheet))
  (%css1-pending 'lass-stylesheet-compile-op))
