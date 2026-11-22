;;;; Backend compile context: the gate, the skip token, the per-form wrapper
;;;; and the signalling helper shared by property functions and variables.
;;;;
;;;; SKIP-INVALID on a value-level failure (a property function or an unbound
;;;; #(name)) cannot remove the declaration from inside LASS, so the failed
;;;; call evaluates to +SKIPPED+, a token no CSS text contains. It flows
;;;; through nested calls and :let* bindings as ordinary text; the walker
;;;; (walk.lisp) drops every declaration (and every selector / prelude item)
;;;; whose text contains it, so the token never reaches css-protocol.

(in-package #:css-backend-lass)

(defvar *in-backend* nil
  "True while a backend compile runs. Backend-only behaviour of LASS methods
\(unbound variables, the skip token) is gated on it, so plain
LASS:COMPILE-SHEET users are unaffected.")

(defvar *include-base* nil
  "The pathname :INCLUDE designators are merged against.")

(defvar *include-stack* '()
  "Truenames of the LASS files being compiled, outermost first.")

(defparameter +skipped+
  (let ((nul (code-char 0)))
    (format nil "~Ccss-backend-lass-skipped~C" nul nul))
  "Text of a value the user skipped (SKIP-INVALID) inside a backend compile.
No letters to upcase, no quote or backslash, no whitespace or comma.")

(defun %skipped-text ()
  "What SKIP-INVALID makes a failed call or reference evaluate to."
  (if *in-backend* +skipped+ ""))

(defun %skipped-p (text)
  "True when TEXT holds a skipped value."
  (and (stringp text) (search +skipped+ text) t))

;;; Value-level failures

(defun %signal-invalid-value (input)
  "Signal CSS-INVALID-VALUE for INPUT with USE-VALUE (the replacement text)
and SKIP-INVALID (the skip token). Returns the text to use."
  (restart-case (error 'css-protocol:css-invalid-value :input input)
    (use-value (text)
      :report "Use replacement text for this value."
      :interactive (lambda ()
                     (format *query-io* "~&Replacement text: ")
                     (force-output *query-io*)
                     (list (read-line *query-io*)))
      text)
    (skip-invalid ()
      :report "Drop every declaration that uses this value."
      (%skipped-text))))

(defun %fail (input)
  "Refuse the current backend property-function call: INPUT is the
offending argument text. Only valid inside WITH-BACKEND-CALL."
  (throw '%backend-call input))

(defmacro with-backend-call ((&rest bindings) &body body)
  "BINDINGS: (var form)*. Each FORM is LASS:RESOLVEd to text, in order. When
one holds the skip token the call is skipped too (nothing is signalled
again). Otherwise BODY runs; a %FAIL in it signals CSS-INVALID-VALUE, whose
USE-VALUE text replaces the whole call."
  (let ((call (gensym "CALL"))
        (input (gensym "INPUT")))
    `(block ,call
       (let* ,(loop for (var form) in bindings
                    collect `(,var (lass:resolve ,form)))
         (when (some #'%skipped-p (list ,@(mapcar #'first bindings)))
           (return-from ,call (%skipped-text)))
         (let ((,input (catch '%backend-call
                         (return-from ,call (progn ,@body)))))
           (%signal-invalid-value ,input))))))

;;; Per top-level form

(defun %compile-form (form)
  "Compile one top-level LASS FORM with LASS:COMPILE-SHEET -> compiled LASS
items. A non-CSS-ERROR error is a LASS-SYNTAX-ERROR (input FORM) with
USE-VALUE (a replacement form, compiled again) and SKIP-INVALID (nothing)."
  (loop
    (restart-case
        (return
          (handler-bind ((error
                           (lambda (e)
                             (unless (typep e 'css-error)
                               (error 'lass-syntax-error :input form :cause e)))))
            (lass:compile-sheet form)))
      (use-value (replacement)
        :report "Compile a replacement LASS form instead."
        :interactive (lambda ()
                       (format *query-io* "~&Replacement form: ")
                       (force-output *query-io*)
                       (list (let ((*read-eval* nil)) (read *query-io*))))
        (setf form replacement))
      (skip-invalid ()
        :report "Drop this LASS form."
        (return '())))))
