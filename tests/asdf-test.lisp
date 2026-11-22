(in-package #:css-backend-lass/tests)

;;; The :lass-stylesheet ASDF component. The fixture system is copied to a
;;; fresh temporary directory, so the test never writes into the checkout.

(defun %fresh-directory ()
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "css-backend-lass-~36R/" (random (expt 36 8)
                                                                             (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun %copy-fixture-system (dir)
  (flet ((copy (from to)
           (let ((target (merge-pathnames to dir)))
             (ensure-directories-exist target)
             (uiop:copy-file (fixture from) target))))
    (copy "asdf/lass-sheet-fixture.asd.in" "lass-sheet-fixture.asd")
    (dolist (name '("sheet" "part" "mini" "plain"))
      (copy (format nil "asdf/src/~A.lass" name) (format nil "src/~A.lass" name)))))

(defun %read-utf-8 (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (let* ((text (make-string (file-length in)))
           (end (read-sequence text in)))
      (subseq text 0 end))))

(defun %octet-length (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (file-length in)))

(defmacro with-fixture-system ((dir) &body body)
  `(let ((,dir (%fresh-directory)))
     (unwind-protect
          (progn
            (%copy-fixture-system ,dir)
            (asdf:load-asd (merge-pathnames "lass-sheet-fixture.asd" ,dir))
            ,@body)
       (asdf:clear-system "lass-sheet-fixture")
       (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore))))

(defparameter *sheet-css*
  (lines "body {"
         "  margin: 0;"
         "}"
         ""
         "a {"
         "  color: #0062CC;"
         (format nil "  content: \"~C\";" (code-char #x2192))
         "}"))

(deftest asdf-component-class
  (with-fixture-system (dir)
    (let ((c (asdf:find-component "lass-sheet-fixture" '("src" "sheet"))))
      (ok (typep c 'lass-stylesheet) ":lass-stylesheet resolves through ASDF/INTERFACE")
      (ok (lass-stylesheet-pretty c))
      (ok (equal "../css/sheet.css" (lass-stylesheet-output c)))
      (ok (null (lass-stylesheet-pretty
                 (asdf:find-component "lass-sheet-fixture" '("src" "mini"))))
          ":pretty defaults to nil"))))

(deftest asdf-component-writes-css
  (with-fixture-system (dir)
    (asdf:load-system "lass-sheet-fixture")
    (let ((sheet (merge-pathnames "css/sheet.css" dir))
          (mini (merge-pathnames "css/mini.css" dir))
          (plain (merge-pathnames "src/plain.css" dir)))
      (ok (probe-file sheet) ":output relative to the component, directories created")
      (ok (and (probe-file sheet) (equal *sheet-css* (%read-utf-8 sheet)))
          "pretty, through the model, includes relative to the component")
      (ok (and (probe-file sheet) (= (+ (length *sheet-css*) 2) (%octet-length sheet)))
          "UTF-8 and LF newlines on every OS (the arrow is 3 octets)")
      (ok (and (probe-file mini)
               (equal "p{margin:8.00px}@media print{p{margin:0}}" (%read-utf-8 mini)))
          "minified by default, no trailing newline")
      (ok (and (probe-file plain) (equal "i{x:y}" (%read-utf-8 plain)))
          "without :output: NAME.css next to the source"))))

(deftest asdf-component-recompiles-over-old-output
  (with-fixture-system (dir)
    (let ((plain (merge-pathnames "src/plain.css" dir)))
      (with-open-file (out plain :direction :output :if-exists :supersede)
        (write-string "stale" out))
      ;; PERFORM directly: :FORCE is refused in an OPERATE nested in TEST-OP.
      (asdf:perform (asdf:make-operation 'asdf:compile-op)
                    (asdf:find-component "lass-sheet-fixture" '("src" "plain")))
      (ok (equal "i{x:y}" (%read-utf-8 plain)) "an existing output is superseded"))))
