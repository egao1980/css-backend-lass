;;;; Colours with SCSS semantics, in exact rationals: parsing, RGB <-> HSL,
;;;; darken / lighten, fade-in / fade-out, mix, printing.
;;;;
;;;; A colour is a list (R G B A): channels R G B rationals in [0, 255],
;;;; alpha A a rational in [0, 1].

(in-package #:css-backend-lass)

(defun %hex-digit (c)
  (digit-char-p c 16))

(defun %parse-hex-colour (text)
  "#rgb #rgba #rrggbb #rrggbbaa -> colour, or NIL."
  (let ((n (length text)))
    (when (and (member n '(4 5 7 9))
               (char= (char text 0) #\#)
               (every #'%hex-digit (subseq text 1)))
      (let ((digits (map 'list #'%hex-digit (subseq text 1))))
        (multiple-value-bind (channels)
            (if (<= n 5)
                (mapcar (lambda (d) (+ (* 16 d) d)) digits)
                (loop for (hi lo) on digits by #'cddr
                      collect (+ (* 16 hi) lo)))
          (destructuring-bind (r g b &optional (a 255)) channels
            (list r g b (/ a 255))))))))

(defun %split-on-commas (text)
  (loop for start = 0 then (1+ comma)
        for comma = (position #\, text :start start)
        collect (string-trim " " (subseq text start comma))
        while comma))

(defun %parse-functional-colour (text)
  "rgb(r, g, b) / rgba(r, g, b, a) with exact decimals -> colour, or NIL."
  (flet ((try (prefix count)
           (let ((p (length prefix)))
             (when (and (> (length text) (1+ p))
                        (string-equal prefix text :end2 p)
                        (char= (char text (1- (length text))) #\)))
               (let ((parts (%split-on-commas (subseq text p (1- (length text))))))
                 (when (= (length parts) count)
                   (let ((numbers (loop for part in parts
                                        collect (multiple-value-bind (v unit ok)
                                                    (%read-decimal part)
                                                  (if (and ok (null unit))
                                                      v
                                                      (return-from try nil))))))
                     (destructuring-bind (r g b &optional (a 1)) numbers
                       (when (and (every (lambda (c) (<= 0 c 255)) (list r g b))
                                  (<= 0 a 1))
                         (list r g b a))))))))))
    (or (try "rgb(" 3) (try "rgba(" 4))))

(defun %colour (text)
  "TEXT read as a colour; %FAIL with TEXT when it is not one."
  (or (and (stringp text)
           (plusp (length text))
           (if (char= (char text 0) #\#)
               (%parse-hex-colour text)
               (%parse-functional-colour text)))
      (%fail text)))

(defun %fraction (text)
  "An amount or weight: N% is N/100, a unitless number is itself."
  (destructuring-bind (value . unit) (%quantity text)
    (case unit
      ((nil) value)
      (:% (/ value 100))
      (t (%fail text)))))

(defun %clamp (x lo hi)
  (max lo (min hi x)))

;;; RGB <-> HSL (all in [0, 1])

(defun %rgb-to-hsl (r g b)
  (let* ((max (max r g b))
         (min (min r g b))
         (l (/ (+ max min) 2)))
    (if (= max min)
        (values 0 0 l)
        (let* ((d (- max min))
               (s (if (> l 1/2) (/ d (- 2 max min)) (/ d (+ max min))))
               (h (cond ((= max r) (+ (/ (- g b) d) (if (< g b) 6 0)))
                        ((= max g) (+ (/ (- b r) d) 2))
                        (t (+ (/ (- r g) d) 4)))))
          (values (/ h 6) s l)))))

(defun %hue-to-rgb (m1 m2 h)
  (let ((h (cond ((minusp h) (1+ h)) ((> h 1) (1- h)) (t h))))
    (cond ((< (* h 6) 1) (+ m1 (* (- m2 m1) h 6)))
          ((< (* h 2) 1) m2)
          ((< (* h 3) 2) (+ m1 (* (- m2 m1) (- 2/3 h) 6)))
          (t m1))))

(defun %hsl-to-rgb (h s l)
  (let* ((m2 (if (<= l 1/2) (* l (+ s 1)) (- (+ l s) (* l s))))
         (m1 (- (* 2 l) m2)))
    (values (%hue-to-rgb m1 m2 (+ h 1/3))
            (%hue-to-rgb m1 m2 h)
            (%hue-to-rgb m1 m2 (- h 1/3)))))

(defun %adjust-lightness (colour delta)
  (destructuring-bind (r g b a) colour
    (multiple-value-bind (h s l) (%rgb-to-hsl (/ r 255) (/ g 255) (/ b 255))
      (multiple-value-bind (r g b) (%hsl-to-rgb h s (%clamp (+ l delta) 0 1))
        (list (* r 255) (* g 255) (* b 255) a)))))

(defun %adjust-alpha (colour delta)
  (destructuring-bind (r g b a) colour
    (list r g b (%clamp (+ a delta) 0 1))))

(defun %mix (c1 c2 p)
  "SCSS's mix: P is C1's share."
  (destructuring-bind ((r1 g1 b1 a1) (r2 g2 b2 a2)) (list c1 c2)
    (let* ((w (- (* 2 p) 1))
           (a (- a1 a2))
           (w1 (/ (+ (if (= (* w a) -1) w (/ (+ w a) (+ 1 (* w a)))) 1) 2))
           (w2 (- 1 w1)))
      (list (+ (* r1 w1) (* r2 w2))
            (+ (* g1 w1) (* g2 w2))
            (+ (* b1 w1) (* b2 w2))
            (+ (* a1 p) (* a2 (- 1 p)))))))

;;; Printing

(defun %round-half-up (x)
  (floor (+ x 1/2)))

(defun %alpha-text (a)
  "At most 4 decimals, rounded half up, trailing zeros dropped."
  (multiple-value-bind (int frac) (floor (%round-half-up (* a 10000)) 10000)
    (if (zerop frac)
        (format nil "~D" int)
        (format nil "~D.~A" int (string-right-trim "0" (format nil "~4,'0D" frac))))))

(defun %colour-text (colour)
  "#RRGGBB when opaque, else rgba(r, g, b, a)."
  (destructuring-bind (r g b a) colour
    (let ((r (%round-half-up r)) (g (%round-half-up g)) (b (%round-half-up b)))
      (if (= a 1)
          (format nil "#~:@(~2,'0X~2,'0X~2,'0X~)" r g b)
          (format nil "rgba(~D, ~D, ~D, ~A)" r g b (%alpha-text a))))))

(defun %rgba-text (colour)
  "Always rgba(r, g, b, a)."
  (destructuring-bind (r g b a) colour
    (format nil "rgba(~D, ~D, ~D, ~A)"
            (%round-half-up r) (%round-half-up g) (%round-half-up b) (%alpha-text a))))
