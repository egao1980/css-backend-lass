;;;; Conditions. GIVEN (not stubs), except %CSS1-PENDING, the stub marker.
;;;;
;;;; css-backend-lass has no error dialect of its own: everything it signals is
;;;; a css-protocol condition, and the three classes below are subclasses of
;;;; CSS-PROTOCOL:CSS-SYNTAX-ERROR (so CSS-ERROR-INPUT works and a handler on
;;;; CSS-ERROR sees them). Every condition the backend signals is signalled
;;;; inside RESTART-CASE with css-protocol's two restarts:
;;;;
;;;;   USE-VALUE (value)  - use VALUE instead; it is processed again (it may
;;;;                        signal again). VALUE's kind is given per case below.
;;;;   SKIP-INVALID ()    - drop the offending item and go on.
;;;;
;;;; What is signalled where (the tests are the exact contract):
;;;;
;;;; - LASS-SYNTAX-ERROR: LASS (or a backend special block / property function
;;;;   given malformed arguments) signals a non-CSS-ERROR error while compiling
;;;;   a top-level form. INPUT = that top-level form (the form of the file being
;;;;   compiled, for an included file); CAUSE = the original condition.
;;;;   USE-VALUE takes a replacement LASS form, compiled in its place;
;;;;   SKIP-INVALID drops the form. Top-level forms are compiled one at a time
;;;;   so the restarts apply per form.
;;;; - LASS-FILE-ERROR: a LASS file cannot be opened or read (missing file,
;;;;   reader error, #. with *READ-EVAL* NIL). INPUT = the path designator as
;;;;   written (the :INCLUDE argument, or the COMPILE-LASS-FILE argument);
;;;;   PATHNAME = the resolved pathname; CAUSE = the underlying condition.
;;;;   USE-VALUE takes another path designator (resolved against the same
;;;;   base); SKIP-INVALID makes the file contribute nothing.
;;;; - LASS-INCLUDE-CYCLE (a LASS-FILE-ERROR): an :INCLUDE of a file that is
;;;;   already being compiled. CHAIN = the pathnames from the outermost file to
;;;;   the repeated one, inclusive (a.lass b.lass a.lass). Same restarts.
;;;; - CSS-PROTOCOL:CSS-INVALID-VALUE from a backend property function (unit
;;;;   arithmetic, colours, ceil/ceiling) or from an unbound #(name) variable:
;;;;   INPUT = the offending argument as text ("1em", "#zzz", the variable
;;;;   name). USE-VALUE takes the replacement text for the call / reference;
;;;;   SKIP-INVALID drops every declaration whose value contains the failed
;;;;   call (also through a :let* binding).
;;;; - CSS-PROTOCOL:CSS-INVALID-VALUE for a compiled (:text ...) object and
;;;;   CSS-PROTOCOL:CSS-INVALID-PRELUDE for an unsupported at-rule (@page,
;;;;   @charset, @namespace, @document, any other top-level property than
;;;;   @import): INPUT = the text LASS would have written for it ("@page
;;;;   :first", "@charset utf-8"). USE-VALUE takes a CSS-NODE or a list of
;;;;   them, placed where the item was (anything else: TYPE-ERROR);
;;;;   SKIP-INVALID drops it.
;;;; - Conditions from css-protocol's constructors (CSS-INVALID-SELECTOR,
;;;;   -PROPERTY, -VALUE, -PRELUDE, -IDENTIFIER) pass through unchanged, with
;;;;   css-protocol's own restarts.

(in-package #:css-backend-lass)

(define-condition lass-syntax-error (css-syntax-error)
  ((cause :initarg :cause :reader lass-syntax-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "LASS could not compile ~S~@[: ~A~]"
                     (css-protocol:css-error-input c)
                     (lass-syntax-error-cause c))))
  (:documentation "LASS signalled an error compiling a top-level form (INPUT)."))

(define-condition lass-file-error (css-syntax-error)
  ((pathname :initarg :pathname :reader lass-file-error-pathname :initform nil)
   (cause :initarg :cause :reader lass-file-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "Cannot read LASS file ~S~@[ (~A)~]~@[: ~A~]"
                     (css-protocol:css-error-input c)
                     (lass-file-error-pathname c)
                     (lass-file-error-cause c))))
  (:documentation "A LASS file could not be opened or read."))

(define-condition lass-include-cycle (lass-file-error)
  ((chain :initarg :chain :reader lass-include-chain :initform nil))
  (:report (lambda (c s)
             (format s "LASS :include cycle: ~{~A~^ -> ~}"
                     (mapcar #'file-namestring (lass-include-chain c)))))
  (:documentation "An :include of a file that is already being compiled."))

;;; Contract stub marker. Every call site is replaced by the H4h-0b-1
;;; implementation; nothing may call it when the slice is done.

(define-condition css1-pending (error)
  ((what :initarg :what :reader css1-pending-what))
  (:report (lambda (c s)
             (format s "H4h-0b-1 contract stub not implemented: ~S"
                     (css1-pending-what c)))))

(defun %css1-pending (what)
  "Contract stub. Every call site is replaced by the H4h-0b-1 implementation."
  (error 'css1-pending :what what))
