;;;; Walk LASS's compiled tree (:block / :superblock / :property / :text, as
;;;; documented in LASS writer.lisp) into css-protocol model nodes.
;;;;
;;;; Text is only ever produced by LASS:WRITE-SHEET-OBJECT (*PRETTY* NIL) and
;;;; handed to css-protocol's constructors, which validate it. Items whose
;;;; text holds the skip token (context.lisp) were skipped by the user and
;;;; produce nothing.

(in-package #:css-backend-lass)

(defun %nodes (value)
  "USE-VALUE of an unsupported item: a CSS-NODE or a list of them."
  (cond ((typep value 'css-protocol:css-node) (list value))
        ((and (listp value) (every (lambda (x) (typep x 'css-protocol:css-node)) value))
         value)
        (t (error 'type-error :datum value
                              :expected-type '(or css-protocol:css-node list)))))

(defun %invalid-item (class input)
  "Signal CLASS (CSS-INVALID-VALUE or CSS-INVALID-PRELUDE) for an item the
model does not hold. -> the nodes to put in its place."
  (restart-case (error class :input input)
    (use-value (value)
      :report "Use css-protocol nodes in place of this item."
      :interactive (lambda ()
                     (format *query-io* "~&Nodes (evaluated): ")
                     (force-output *query-io*)
                     (list (eval (read *query-io*))))
      (%nodes value))
    (skip-invalid ()
      :report "Drop this item."
      '())))

(defun %write-object (type object)
  (with-output-to-string (s)
    (let ((lass:*pretty* nil))
      (lass:write-sheet-object type object s))))

(defun %prelude (selector)
  "Text of a superblock's (:selector . constraints), or \"\"."
  (if selector (%selector-text (rest selector)) ""))

(defun %literals (selector)
  "The texts of the (:constraint :literal text) items of a backend superblock."
  (mapcar #'third (rest selector)))

;;; Declarations

(defun %whitespace-p (c)
  (member c '(#\Space #\Tab #\Return #\Newline #\Page)))

(defun %split-important (value)
  "-> (values VALUE IMPORTANT-P): a trailing !important (any case, optional
whitespace before it) is split off."
  (let* ((suffix "!important")
         (n (length value))
         (k (length suffix)))
    (if (and (>= n k) (string-equal suffix value :start2 (- n k)))
        (values (string-right-trim '(#\Space #\Tab #\Return #\Newline #\Page)
                                   (subseq value 0 (- n k)))
                t)
        (values value nil))))

(defun %declaration (item)
  "(:property key value) inside a block -> CSS-DECLARATION, or NIL (skipped)."
  (destructuring-bind (key &optional value) (rest item)
    (let ((value (or value "")))
      (unless (or (%skipped-p key) (%skipped-p value))
        (multiple-value-bind (value important) (%split-important value)
          (css-protocol:make-declaration key value :important important))))))

(defun %declarations (items)
  (loop for item in items
        for d = (if (and (consp item) (eq (first item) :property))
                    (%declaration item)
                    (first (%invalid-item 'css-protocol:css-invalid-value
                                          (%write-object (first item) (rest item)))))
        when d collect d))

;;; @import

(defun %import-rule (key)
  "KEY = \"@import URL[ media]\" (what LASS writes). The URL token is
url(\"u\"), url(u), \"u\" or a bare u; the rest is the media."
  (let* ((text (string-trim " " (subseq key (length "@import"))))
         (n (length text))
         (end nil)
         (url nil))
    (cond ((and (>= n 4) (string-equal "url(" text :end2 4))
           (setf end (or (position #\) text) n)
                 url (string-trim "\"'" (subseq text 4 end)))
           (setf end (min n (1+ end))))
          ((and (plusp n) (char= (char text 0) #\"))
           (setf end (let ((close (position #\" text :start 1)))
                       (if close (1+ close) n))
                 url (string-trim "\"" (subseq text 0 end))))
          (t
           (setf end (or (position-if #'%whitespace-p text) n)
                 url (subseq text 0 end))))
    (let ((media (string-trim " " (subseq text end))))
      (css-protocol:make-import-rule url :media (if (string= media "") nil media)))))

;;; Items

(defun %block-node (item)
  "(:block (:selector . constraints) . properties) -> nodes."
  (destructuring-bind (selector &rest properties) (rest item)
    (when properties
      (let ((text (%selector-text (rest selector))))
        (cond ((%skipped-p text) '())
              ((string= text "@font-face")
               (let ((declarations (%declarations properties)))
                 (when declarations
                   (list (css-protocol:make-font-face-rule declarations)))))
              ((and (plusp (length text)) (char= (char text 0) #\@))
               (%invalid-item 'css-protocol:css-invalid-prelude text))
              (t
               (let ((declarations (%declarations properties)))
                 (when declarations
                   (let ((rule (css-protocol:make-rule text declarations)))
                     (when rule (list rule)))))))))))

(defun %keyframes-node (name blocks)
  (let ((keyframes
          (loop for block in blocks
                append (if (and (consp block) (eq (first block) :block))
                           (destructuring-bind (selector &rest properties) (rest block)
                             (let ((text (%selector-text (rest selector))))
                               (unless (%skipped-p text)
                                 (let ((declarations (%declarations properties)))
                                   (when declarations
                                     (let ((k (css-protocol:make-keyframe text declarations)))
                                       (when k (list k))))))))
                           (%walk-items (list block))))))
    (when keyframes
      (let ((rule (css-protocol:make-keyframes-rule name keyframes)))
        (when rule (list rule))))))

(defun %superblock-node (item)
  "(:superblock type selector . blocks) -> nodes."
  (destructuring-bind (type selector &rest blocks) (rest item)
    (let ((prelude (%prelude selector)))
      (flet ((group (constructor)
               (let ((children (%walk-items blocks)))
                 (when children
                   (let ((rule (funcall constructor prelude children)))
                     (when rule (list rule)))))))
        (cond ((%skipped-p prelude) '())
              ((equal type "media") (group #'css-protocol:make-media-rule))
              ((equal type "supports") (group #'css-protocol:make-supports-rule))
              ((equal type "keyframes") (%keyframes-node prelude blocks))
              ((equal type "layer")
               (let ((names (%literals selector)))
                 (if blocks
                     (group (lambda (prelude children)
                              (declare (ignore prelude))
                              (css-protocol:make-layer-rule names children)))
                     (let ((rule (css-protocol:make-layer-rule names)))
                       (when rule (list rule))))))
              ((equal type "scope")
               (destructuring-bind (start &optional end) (%literals selector)
                 (group (lambda (prelude children)
                          (declare (ignore prelude))
                          (css-protocol:make-scope-rule start children :end end)))))
              (t
               (%invalid-item 'css-protocol:css-invalid-prelude
                              (format nil "@~A~:[ ~A~;~*~]"
                                      type (string= prelude "") prelude))))))))

(defun %property-node (item top)
  "A (:property key value) outside a block: only a top-level @import."
  (destructuring-bind (key &optional value) (rest item)
    (cond ((or (%skipped-p key) (%skipped-p value)) '())
          ((and top (null value) (stringp key)
                (> (length key) 8) (string-equal "@import " key :end2 8))
           (let ((rule (%import-rule key)))
             (when rule (list rule))))
          (t (%invalid-item 'css-protocol:css-invalid-prelude
                            (if value (format nil "~A ~A" key value) key))))))

(defun %walk-items (items &key top)
  "Compiled LASS ITEMS -> a list of css-protocol nodes, in order."
  (loop for item in items
        append (if (consp item)
                   (case (first item)
                     (:block (%block-node item))
                     (:superblock (%superblock-node item))
                     (:property (%property-node item top))
                     (:text (%invalid-item 'css-protocol:css-invalid-value
                                           (format nil "~{~A~}" (rest item))))
                     (t (%invalid-item 'css-protocol:css-invalid-prelude
                                       (%write-object (first item) (rest item)))))
                   (%invalid-item 'css-protocol:css-invalid-prelude
                                  (princ-to-string item)))))
