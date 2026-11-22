(in-package #:css-backend-lass/tests)

;;; The backend is registered with css-protocol: COMPILE-STYLESHEET reaches it
;;; through :backend, *css-backend*, or a LASS-SOURCE.

(deftest backend-instance
  (ok (typep *lass-backend* 'lass-backend))
  (ok (typep *lass-backend* 'css-backend))
  (ok (not (typep *css-backend* 'lass-backend)) "loading does not set *css-backend*"))

(deftest compile-stylesheet-with-the-backend
  (let ((*css-backend* nil))
    (ok (equal "a{color:red}"
               (write-stylesheet (compile-stylesheet '((a :color red)) :backend *lass-backend*)
                                 :pretty nil)))
    (ok (equal "a{color:red}"
               (write-stylesheet (compile-stylesheet '((a :color #(c)))
                                                     :backend *lass-backend*
                                                     :variables '((c . "red")))
                                 :pretty nil))
        ":variables passes through compile-stylesheet"))
  (let ((*css-backend* *lass-backend*))
    (ok (equal "a{color:red}"
               (write-stylesheet (compile-stylesheet '((a :color red))) :pretty nil))
        "as the default backend")))

(deftest lass-source-dispatch
  (let ((*css-backend* nil))
    (ok (equal "a{color:red}"
               (write-stylesheet (compile-stylesheet (make-lass-source :forms '((a :color red))))
                                 :pretty nil))
        "a lass-source needs no *css-backend*")
    (ok (equal "body{color:#111}p{padding:4px;margin:8.00px}"
               (write-stylesheet (compile-stylesheet (make-lass-source :pathname (fixture "main.lass")))
                                 :pretty nil))
        "a lass-source of a file")
    (ok (equal "p{padding:1px;margin:2.00px}"
               (write-stylesheet
                (compile-stylesheet (make-lass-source :forms '((:include "parts/nested/pad.lass"))
                                                      :pathname (fixture "main.lass")
                                                      :variables '(("pad" . "1px"))))
                :pretty nil))
        "forms with pathname as the include base, and variables")
    (let ((source (make-lass-source :forms '((a :x y)) :variables '((v . "1")))))
      (ok (equal '((a :x y)) (lass-source-forms source)))
      (ok (equal '((v . "1")) (lass-source-variables source)))
      (ok (null (lass-source-pathname source))))))

(deftest backend-compile-stylesheet-sources
  (ok (equal "body{color:#111}p{padding:4px;margin:8.00px}"
             (write-stylesheet (backend-compile-stylesheet *lass-backend* (fixture "main.lass"))
                               :pretty nil))
      "a pathname is a LASS file")
  (ok (equal "a{x:y}"
             (write-stylesheet (backend-compile-stylesheet *lass-backend* '((a :x y))) :pretty nil)))
  (ok (signals (backend-compile-stylesheet *lass-backend* 42) 'type-error))
  (ok (signals (backend-compile-stylesheet *lass-backend* "(a :x y)") 'type-error)
      "LASS text is not parsed"))

(deftest compile-lass-is-compile-stylesheet
  (let ((forms '((a :color red (b :margin 0)) (:media print (a :color black)))))
    (ok (equal (write-stylesheet (compile-lass forms))
               (write-stylesheet (compile-stylesheet forms :backend *lass-backend*))))))
