;;;; ASDF component :LASS-STYLESHEET.
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
  (let* ((output (first (asdf:output-files op c)))
         (text (css-protocol:write-stylesheet
                (compile-lass-file (asdf:component-pathname c))
                :pretty (lass-stylesheet-pretty c)))
         ;; #\Newline is LF in every Lisp string; writing octets keeps it LF
         ;; (no OS newline translation) and the encoding UTF-8.
         (octets (encoding-protocol:encode text :encoding :utf-8)))
    (ensure-directories-exist output)
    (with-open-file (out output :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (write-sequence octets out))
    output))
