;;;; The LASS backend: the classes, the compile driver and the entry points.
;;;; The tests are the exact contract; this header is
;;;; the summary.
;;;;
;;;; COMPILE PATH (locked by the brief)
;;;;   Each top-level form goes through LASS:COMPILE-SHEET (one form at a time,
;;;;   so restarts apply per form) with LASS:*VARS* bound to a fresh table
;;;;   seeded from :VARIABLES (the global table is never read or written).
;;;;   The compiled tree is walked into the model:
;;;;   - (:block (:selector . c) props...) -> MAKE-RULE. Selector text is
;;;;     LASS:WRITE-SHEET-OBJECT of the :selector with LASS:*PRETTY* NIL, given
;;;;     to css-protocol (PARSE-SELECTOR-LIST / MAKE-RULE). Never parse CSS.
;;;;   - (:property k v) -> MAKE-DECLARATION. A value ending in "!important"
;;;;     (case-insensitive, optional whitespace before it) becomes
;;;;     :IMPORTANT T with the rest of the text as the value.
;;;;   - superblock "media" / "supports" -> MEDIA-RULE / SUPPORTS-RULE; the
;;;;     prelude is the :selector written like a selector (*PRETTY* NIL):
;;;;     (:max-width 400px) -> "(max-width:400px)".
;;;;   - superblock "keyframes" -> KEYFRAMES-RULE (name = the written prelude)
;;;;     with one KEYFRAME per block (selectors = the written selector).
;;;;   - a block whose written selector is "@font-face" -> FONT-FACE-RULE.
;;;;   - a top-level (:property "@import ..." nil) -> IMPORT-RULE: the first
;;;;     token is the URL (url("u"), "u" or bare u), the rest is the media.
;;;;   - the backend's own superblocks "layer" and "scope" -> LAYER-RULE /
;;;;     SCOPE-RULE (see the special blocks below).
;;;;   - rules, @font-face and keyframes with no declarations left are
;;;;     dropped (LASS writes nothing for them); grouping rules left with no
;;;;     children are dropped too (a @layer statement is not a group).
;;;;   - anything else: see conditions.lisp (:text, @page, @charset, ...).
;;;;   - a #(name) reference with no binding (LASS would write "NIL") is a
;;;;     CSS-INVALID-VALUE inside backend compiles (input = the downcased name).
;;;;
;;;; SPECIAL BLOCKS added to LASS (global LASS tables; nothing else global)
;;;;   (:let* ((name value-form)...) body...)  sequential, like CL LET*; each
;;;;       value is LASS:RESOLVEd once, seeing earlier bindings; shadowing ends
;;;;       with the body. A binding that is not (name value-form) is a
;;;;       LASS-SYNTAX-ERROR.
;;;;   (:include "file.lass")  compiles that file in place. Relative to the
;;;;       including file (or :PATHNAME, else *DEFAULT-PATHNAME-DEFAULTS*);
;;;;       current bindings are visible in it; its own :let* do not leak out.
;;;;       Read as UTF-8 with *PACKAGE* = CSS-BACKEND-LASS-USER and
;;;;       *READ-EVAL* NIL (standard readtable).
;;;;       Cycles -> LASS-INCLUDE-CYCLE; including a file twice is fine.
;;;;   (:layer names body...)  names: a name or a list of names (symbols are
;;;;       downcased), NIL = anonymous. No body -> @layer statement.
;;;;   (:scope start body...) / (:scope (:to start end) body...)  -> @scope.
;;;;
;;;; PROPERTY FUNCTIONS added to LASS (classie's, re-implemented, no deps)
;;;;   Arguments are LASS:RESOLVEd to text, then read as exact decimals:
;;;;   [+-]digits[.digits] or [+-].digits, then an optional unit px pt in %.
;;;;   Anything else -> CSS-INVALID-VALUE (input = that text). Exact rationals
;;;;   throughout (no float noise): 0.85 is 17/20.
;;;;   + - * /   1in = 96px = 72pt. Lengths in + and - convert to the left
;;;;       operand's unit; a unitless operand takes the other's unit (SCSS);
;;;;       % * length = length * p/100; % * % = % (p1*p2/100); % / % and
;;;;       length / length are unitless; length / % = length / (p/100).
;;;;       % +- length, length * length, n / length, n / %, / 0 -> invalid.
;;;;   ceil     CEILING of the shown number: 49.5% -> 50.00%.
;;;;   ceiling  CEILING of the value as a fraction: 49.5% -> 100.00%.
;;;;   values   resolved arguments joined by one space.
;;;;   Output: with a unit, two decimals rounded half away from zero (14.00px,
;;;;   9.08in, 25.00%) -- classie's format, kept for engels; unitless, an
;;;;   integer when integral (2) else two decimals (0.33).
;;;;   darken / lighten (color amount)  HSL lightness -/+ amount, clamped.
;;;;   fade-in / fade-out (color amount)  alpha +/- amount, clamped -> always
;;;;       rgba(r, g, b, a).
;;;;   mix (a b &optional weight)  SCSS's weighted mix (alpha-aware), weight =
;;;;       share of A, default 50%.
;;;;   Amounts and weights: N% is N/100, a unitless number is the fraction
;;;;   itself; weight outside [0, 1] -> invalid. Colours: #rgb #rgba #rrggbb
;;;;   #rrggbbaa, rgb(r, g, b), rgba(r, g, b, a); names and anything else ->
;;;;   invalid. Result: #RRGGBB (uppercase, as classie) when opaque, else
;;;;   rgba(r, g, b, a); channels rounded half up; alpha at most 4 decimals,
;;;;   trailing zeros dropped (0.25, 0.402, 1, 0).
;;;;   calc keeps LASS 0.6.0's output exactly: inside (calc ...) the
;;;;   arithmetic operators are calc operators, not evaluated.

(in-package #:css-backend-lass)

(defclass lass-backend (css-protocol:css-backend) ()
  (:documentation "css-protocol backend for LASS sources."))

(defvar *lass-backend* (make-instance 'lass-backend)
  "The LASS backend instance. Loading this system does not change
CSS-PROTOCOL:*CSS-BACKEND*.")

(defclass lass-source ()
  ((forms :initarg :forms :initform nil :reader lass-source-forms)
   (pathname :initarg :pathname :initform nil :reader lass-source-pathname)
   (variables :initarg :variables :initform nil :reader lass-source-variables))
  (:documentation "A LASS source for COMPILE-STYLESHEET: FORMS (compiled with
PATHNAME as the :include base) or, with no FORMS, the file at PATHNAME.
COMPILE-STYLESHEET dispatches it to the LASS backend whatever
CSS-PROTOCOL:*CSS-BACKEND* is."))

(defun make-lass-source (&key forms pathname variables)
  "A LASS-SOURCE. VARIABLES is an alist as for COMPILE-LASS."
  (make-instance 'lass-source :forms forms :pathname pathname :variables variables))

;;; Compile driver

(defun %variable-key (name)
  "A :VARIABLES key: a symbol as is, a string interned upcased in
CSS-BACKEND-LASS-USER."
  (if (stringp name)
      (intern (string-upcase name) '#:css-backend-lass-user)
      name))

(defun %fresh-vars (variables)
  "A fresh LASS variable table seeded from the VARIABLES alist (values
LASS:RESOLVEd, in order, each seeing the earlier ones)."
  (let ((table (make-hash-table)))
    (let ((lass:*vars* table))
      (loop for (name . value) in variables
            do (setf (gethash (%variable-key name) table) (lass:resolve value))))
    table))

(defmacro with-backend-compile ((&key variables (base '*default-pathname-defaults*)
                                   (stack ''()))
                                &body body)
  "Run BODY as a backend compile: the gate on, LASS:*VARS* a fresh table
from VARIABLES (the global table is never read or written), the include
base and stack bound."
  `(let* ((*in-backend* t)
          (*include-base* ,base)
          (*include-stack* ,stack)
          (lass:*vars* (%fresh-vars ,variables)))
     ,@body))

(defun %walk-forms (forms)
  "Compile each top-level form on its own, then walk it into nodes."
  (loop for form in forms
        append (%walk-items (%compile-form form) :top t)))

(defun %compile-forms (forms variables pathname)
  (check-type forms list)
  (with-backend-compile (:variables variables
                         :base (or pathname *default-pathname-defaults*))
    (css-protocol:make-stylesheet (%walk-forms forms))))

(defun %compile-file (pathname variables)
  (with-backend-compile (:variables variables)
    (multiple-value-bind (forms file) (%read-lass-file pathname *default-pathname-defaults*)
      (css-protocol:make-stylesheet
       (when file
         (let ((*include-base* file)
               (*include-stack* (list file)))
           (%walk-forms forms)))))))

(defmethod css-protocol:backend-compile-stylesheet
    ((backend lass-backend) source &key variables pathname &allow-other-keys)
  "SOURCE: a list of LASS forms, a pathname (a LASS file) or a LASS-SOURCE.
Anything else (strings included: LASS text is not parsed) is a TYPE-ERROR."
  (etypecase source
    (list (%compile-forms source variables pathname))
    (pathname (%compile-file source variables))
    (lass-source
     (let ((variables (or variables (lass-source-variables source)))
           (pathname (or pathname (lass-source-pathname source))))
       (if (or (lass-source-forms source) (null pathname))
           (%compile-forms (lass-source-forms source) variables pathname)
           (%compile-file pathname variables))))))

(defmethod css-protocol:compile-stylesheet
    ((source lass-source) &rest keys &key backend &allow-other-keys)
  "A LASS-SOURCE compiles with BACKEND, else *LASS-BACKEND*."
  (apply #'css-protocol:backend-compile-stylesheet
         (or backend *lass-backend*) source
         (loop for (key value) on keys by #'cddr
               unless (eq key :backend) nconc (list key value))))

(defun compile-lass (forms &key variables pathname)
  "Compile the LASS top-level FORMS into a CSS-PROTOCOL:STYLESHEET.
VARIABLES: alist (name . value); a name is a symbol (matched EQ, as LASS
does) or a string (interned upcased in CSS-BACKEND-LASS-USER); a value is
LASS:RESOLVEd. PATHNAME: the file FORMS come from (the :include base).
Same as (compile-stylesheet forms :backend *lass-backend* ...)."
  (css-protocol:compile-stylesheet forms :backend *lass-backend*
                                         :variables variables
                                         :pathname pathname))

(defun compile-lass-file (pathname &key variables)
  "Read the LASS file PATHNAME (CSS-BACKEND-LASS-USER, *READ-EVAL* NIL) and
compile it. Includes are relative to it. A file that cannot be read signals
LASS-FILE-ERROR."
  (%compile-file pathname variables))
