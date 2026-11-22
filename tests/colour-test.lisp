(in-package #:css-backend-lass/tests)

;;; classie's colour functions with SCSS semantics. Expected values are
;;; SCSS's (lowercase hex in SCSS; uppercase here, as classie printed them).
;;; The #007bff rows are Bootstrap 4's $primary shades, which engels uses.

(deftest-parametrize darken-and-lighten
    ((form expected)
     ('(darken "#fff" 20%) "#CCCCCC")
     ('(darken "#fff" 0.2) "#CCCCCC")
     ('(darken "#007bff" 7.5%) "#0069D9")
     ('(darken "#007bff" 10%) "#0062CC")
     ('(darken "#007bff" 12.5%) "#005CBF")
     ('(darken "#007bff" 15%) "#0056B3")
     ('(darken "#f44336" 10%) "#EA1C0D")
     ('(darken "#333" 50%) "#000000")
     ('(darken "#007BFF" 10%) "#0062CC")
     ('(lighten "#007bff" 10%) "#3395FF")
     ('(lighten "#000" 50%) "#808080")
     ('(lighten "#ccc" 50%) "#FFFFFF")
     ('(lighten "#abc" 10%) "#CAD5DF")
     ('(darken "rgb(0, 123, 255)" 10%) "#0062CC")
     ('(darken "rgba(0, 123, 255, 0.5)" 10%) "rgba(0, 98, 204, 0.5)"))
  (ok (equal expected (value-of form)) (format nil "~S" form)))

(deftest-parametrize fade-in-and-out
    ((form expected)
     ('(fade-out "#007bff" 0.75) "rgba(0, 123, 255, 0.25)")
     ('(fade-out "#007bff" 0.5) "rgba(0, 123, 255, 0.5)")
     ('(fade-out "#000" 25%) "rgba(0, 0, 0, 0.75)")
     ('(fade-out "#000" 1.5) "rgba(0, 0, 0, 0)")
     ('(fade-out "#00000080" 0.1) "rgba(0, 0, 0, 0.402)")
     ('(fade-in "rgba(0, 123, 255, 0.5)" 0.25) "rgba(0, 123, 255, 0.75)")
     ('(fade-in "#007bff" 0.5) "rgba(0, 123, 255, 1)")
     ('(fade-in "#0000" 40%) "rgba(0, 0, 0, 0.4)"))
  (ok (equal expected (value-of form)) (format nil "~S" form)))

(deftest-parametrize mix
    ((form expected)
     ('(mix "#007bff" "#fff") "#80BDFF")
     ('(mix "#f00" "#00f") "#800080")
     ('(mix "#f00" "#00f" 25%) "#4000BF")
     ('(mix "#f00" "#00f" 0.75) "#BF0040")
     ('(mix "#f00" "#00f" 100%) "#FF0000")
     ('(mix "#f00" "#00f" 0) "#0000FF")
     ('(mix "rgba(255, 0, 0, 0.5)" "#00f") "rgba(64, 0, 191, 0.75)"))
  (ok (equal expected (value-of form)) (format nil "~S" form)))

(deftest colours-in-context
  ;; engels/src/forms.lass and buttons.lass shapes.
  (ok (equal "input:focus{border-color:#80BDFF;box-shadow:0 0 0 0.2rem rgba(0, 123, 255, 0.25)}"
             (mini '((:let* ((primary "#007bff"))
                       ((:and input :focus)
                        :border-color (mix #(primary) "#fff")
                        :box-shadow 0 0 0 0.2rem (fade-out #(primary) 0.75)))))))
  (ok (equal "a:hover{color:#0056B3}"
             (mini '((:let* ((link-color "#007bff")
                             (link-hover-color (darken #(link-color) 15%)))
                       ((:and a :hover) :color #(link-hover-color))))))
      ":let* binds a colour function result")
  (ok (equal "a{color:#0062CC}" (mini '((a :color (darken (lighten "#007bff" 0%) 10%)))))
      "colour functions nest"))

(deftest-parametrize colour-errors
    ((form input)
     ('(darken "red" 10%) "red")
     ('(darken "#ggg" 10%) "#ggg")
     ('(lighten "#12345" 10%) "#12345")
     ('(darken "#fff" "1px") "1px")
     ('(fade-out "#fff" "x") "x")
     ('(mix "#fff" "nope") "nope")
     ('(mix "#fff" "#000" "2px") "2px")
     ('(mix "#fff" "#000" 150%) "150%")
     ('(darken "rgb(1, 2)" 10%) "rgb(1, 2)"))
  (let ((c (condition-of (lambda () (value-of form)))))
    (ok (typep c 'css-invalid-value) (format nil "~S" form))
    (ok (and c (equal input (css-error-input c))) (format nil "input of ~S" form))))
