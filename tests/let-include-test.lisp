(in-package #:css-backend-lass/tests)

;;; :let*, :variables and :include (relative paths, nesting, cycles).

(deftest let*-sequential
  (ok (equal "p{margin:2.00px;padding:1px}"
             (mini '((:let* ((a "1px")
                             (b (* 2 #(a))))
                       (p :margin #(b) :padding #(a))))))))

(deftest let*-shadowing
  (ok (equal "a{color:red}b{color:blue}i{color:red}"
             (mini '((:let* ((c "red"))
                       (a :color #(c))
                       (:let* ((c "blue")) (b :color #(c)))
                       (i :color #(c)))))))
  (ok (equal "a{color:blue}b{color:red}"
             (mini '((:let ((c "red"))
                       (:let* ((c "blue")) (a :color #(c)))
                       (b :color #(c))))))
      ":let* and LASS's :let see each other"))

(deftest let*-nested-in-blocks-and-media
  (ok (equal "@media print{a{color:#000}}"
             (mini '((:let* ((ink "#000"))
                       (:media print (a :color #(ink)))))))))

(deftest variables-option
  (ok (equal "a{color:red}" (mini '((a :color #(fg))) :variables '((fg . "red")))))
  (ok (equal "a{margin:4px}" (mini '((a :margin #(m))) :variables '((m . 4px))))
      "values are resolved")
  (ok (equal "a{color:blue}"
             (mini '((:let* ((fg "blue")) (a :color #(fg)))) :variables '((fg . "red"))))
      "bindings in the source shadow :variables")
  (ok (equal "a{color:red}" (mini-file "vars.lass" :variables '(("fg" . "red"))))
      "a string key is interned upcased in css-backend-lass-user")
  (ok (equal "a{color:red}"
             (mini-file "vars.lass" :variables '((css-backend-lass-user::fg . "red"))))))

(deftest no-global-variable-state
  (let ((before (hash-table-count lass:*vars*)))
    (compile-lass '((:let* ((zz "1px")) (a :x #(zz)))) :variables '((yy . "2px")))
    (ok (= before (hash-table-count lass:*vars*)))
    (ok (null (nth-value 1 (gethash 'yy lass:*vars*))))
    (ok (null (nth-value 1 (gethash 'zz lass:*vars*)))))
  (let ((c (condition-of (lambda () (compile-lass '((a :x #(yy))))))))
    (ok (typep c 'css-invalid-value) ":variables do not outlive the compile")))

(deftest include-relative-and-nested
  ;; main.lass binds fg / pad and includes parts/base.lass, which includes
  ;; nested/pad.lass relative to parts/.
  (ok (equal "body{color:#111}p{padding:4px;margin:8.00px}" (mini-file "main.lass"))))

(deftest include-from-forms
  (ok (equal "p{padding:1px;margin:2.00px}"
             (mini '((:include "parts/nested/pad.lass"))
                   :pathname (fixture "main.lass")
                   :variables '(("pad" . "1px"))))
      ":pathname is the include base")
  (ok (equal "p{padding:1px;margin:2.00px}"
             (let ((*default-pathname-defaults*
                     (make-pathname :name nil :type nil :defaults (fixture "main.lass"))))
               (mini '((:include "parts/nested/pad.lass"))
                     :variables '(("pad" . "1px")))))
      "without :pathname, relative to *default-pathname-defaults*")
  (ok (equal "@media print{p{padding:1px;margin:2.00px}}"
             (mini '((:media print (:include "parts/nested/pad.lass")))
                   :pathname (fixture "main.lass")
                   :variables '(("pad" . "1px"))))
      "include inside a group"))

(deftest include-scoping
  ;; scoping.lass: (:let* ((fg "blue")) (:include "parts/local.lass") (b ...))
  ;; parts/local.lass binds its own fg for em only.
  (ok (equal "em{color:red}strong{color:blue}b{color:blue}" (mini-file "scoping.lass"))))

(deftest include-twice-is-not-a-cycle
  (ok (equal "p{padding:1px;margin:2.00px}p{padding:1px;margin:2.00px}"
             (mini-file "diamond.lass" :variables '(("pad" . "1px"))))))

(deftest include-cycle
  (let ((c (condition-of (lambda () (compile-lass-file (fixture "cycle-a.lass"))))))
    (ok (typep c 'lass-include-cycle))
    (ok (typep c 'lass-file-error))
    (ok (typep c 'css-syntax-error))
    (when c
      (ok (equal "cycle-a.lass" (css-error-input c)) "input: the include as written")
      (ok (equal '("cycle-a.lass" "cycle-b.lass" "cycle-a.lass")
                 (names (lass-include-chain c))))
      (ok (search "cycle-b.lass" (princ-to-string c)) "the report shows the chain")))
  (let ((c (condition-of (lambda () (compile-lass-file (fixture "self.lass"))))))
    (ok (typep c 'lass-include-cycle))
    (when c (ok (equal '("self.lass" "self.lass") (names (lass-include-chain c))))))
  (ok (equal "a{color:red}b{color:blue}"
             (handler-bind ((lass-include-cycle #'skip-invalid))
               (mini-file "cycle-a.lass")))
      "skip-invalid: the cyclic include contributes nothing"))

(deftest include-missing-file
  (let ((c (condition-of (lambda ()
                           (compile-lass '((:include "nope.lass"))
                                         :pathname (fixture "main.lass"))))))
    (ok (typep c 'lass-file-error))
    (ok (not (typep c 'lass-include-cycle)))
    (when c
      (ok (equal "nope.lass" (css-error-input c)))
      (ok (equal "nope.lass" (file-namestring (lass-file-error-pathname c))))))
  (ok (equal "a{x:y}"
             (handler-bind ((lass-file-error #'skip-invalid))
               (mini '((:include "nope.lass") (a :x y)) :pathname (fixture "main.lass")))))
  (ok (equal "p{padding:1px;margin:2.00px}"
             (handler-bind ((lass-file-error
                              (lambda (c) (use-value "parts/nested/pad.lass" c))))
               (mini '((:include "nope.lass"))
                     :pathname (fixture "main.lass")
                     :variables '(("pad" . "1px")))))
      "use-value: another path, resolved against the same base")
  (ok (typep (condition-of (lambda () (compile-lass-file (fixture "absent.lass"))))
             'lass-file-error)
      "compile-lass-file of a missing file"))

(defvar *evaluated* nil)

(deftest file-reading
  (setf *evaluated* nil)
  (ok (typep (condition-of (lambda () (compile-lass-file (fixture "bad-read.lass"))))
             'lass-file-error)
      "an unbalanced file")
  (let ((c (condition-of (lambda () (compile-lass-file (fixture "read-eval.lass"))))))
    (ok (typep c 'lass-file-error) "#. is refused (*read-eval* nil)")
    (ok (null *evaluated*))
    (when c (ok (typep (lass-file-error-cause c) 'error))))
  (ok (equal "" (handler-bind ((lass-file-error #'skip-invalid))
                  (mini-file "bad-read.lass")))
      "skip-invalid on the top-level file: an empty sheet"))
