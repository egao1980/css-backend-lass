;;;; Exact decimal quantities: reader, printer, unit arithmetic (classie's
;;;; + - * /, ceil, ceiling), all in rationals.
;;;;
;;;; A quantity is (VALUE . UNIT): VALUE a rational, UNIT NIL (unitless), :PX,
;;;; :PT, :IN (lengths) or :% (percent).

(in-package #:css-backend-lass)

(defparameter +units+ '(("px" . :px) ("pt" . :pt) ("in" . :in) ("%" . :%))
  "Unit suffixes the arithmetic reads.")

(defun %unit-px (unit)
  "Size of one UNIT in px (1in = 96px = 72pt)."
  (ecase unit (:px 1) (:pt 4/3) (:in 96)))

(defun %length-unit-p (unit)
  (member unit '(:px :pt :in)))

(defun %digit-run-end (text start)
  (or (position-if-not (lambda (c) (char<= #\0 c #\9)) text :start start)
      (length text)))

(defun %read-decimal (text)
  "TEXT = [+-]digits[.digits] or [+-].digits, then an optional unit.
-> (values VALUE UNIT T), or NIL when TEXT is not a quantity."
  (when (stringp text)
    (let* ((n (length text))
           (i 0)
           (sign 1))
      (when (and (< i n) (find (char text i) "+-"))
        (when (char= (char text i) #\-) (setf sign -1))
        (incf i))
      (let* ((int-end (%digit-run-end text i))
             (int-digits (- int-end i))
             (frac-start int-end)
             (frac-end frac-start))
        (when (and (< int-end n) (char= (char text int-end) #\.))
          (setf frac-start (1+ int-end)
                frac-end (%digit-run-end text frac-start))
          (when (= frac-end frac-start)
            (return-from %read-decimal nil)))
        (when (zerop (+ int-digits (- frac-end frac-start)))
          (return-from %read-decimal nil))
        (let* ((int (if (plusp int-digits) (parse-integer text :start i :end int-end) 0))
               (frac-digits (- frac-end frac-start))
               (frac (if (plusp frac-digits)
                         (/ (parse-integer text :start frac-start :end frac-end)
                            (expt 10 frac-digits))
                         0))
               (suffix (subseq text frac-end))
               (unit (if (string= suffix "")
                         nil
                         (or (cdr (assoc suffix +units+ :test #'string-equal))
                             (return-from %read-decimal nil)))))
          (values (* sign (+ int frac)) unit t))))))

(defun %quantity (text)
  "TEXT read as a quantity (VALUE . UNIT); %FAIL with TEXT when it is not."
  (multiple-value-bind (value unit ok) (%read-decimal text)
    (if ok (cons value unit) (%fail text))))

(defun %round-half-away (x)
  "X rounded to an integer, halves away from zero."
  (if (minusp x)
      (- (floor (+ (- x) 1/2)))
      (floor (+ x 1/2))))

(defun %two-decimals (x)
  "X with exactly two decimals, rounded half away from zero."
  (let* ((n (%round-half-away (* x 100)))
         (a (abs n)))
    (multiple-value-bind (int frac) (floor a 100)
      (format nil "~:[~;-~]~D.~2,'0D" (minusp n) int frac))))

(defun %unit-text (unit)
  (car (rassoc unit +units+)))

(defun %quantity-text (q)
  "Print quantity Q: with a unit, two decimals; unitless, an integer when
integral, else two decimals."
  (destructuring-bind (value . unit) q
    (cond (unit (concatenate 'string (%two-decimals value) (%unit-text unit)))
          ((integerp value) (format nil "~D" value))
          (t (%two-decimals value)))))

(defun %convert (value from to)
  "VALUE in length unit FROM, expressed in length unit TO."
  (/ (* value (%unit-px from)) (%unit-px to)))

;;; Operators. A and B are quantities; BTEXT is B's text (the input of an
;;; incompatible-unit or zero-divisor failure).

(defun %add (a b btext sign)
  (destructuring-bind ((av . au) (bv . bu)) (list a b)
    (flet ((result (v u) (cons (+ av (* sign v)) u)))
      (cond ((eq au bu) (result bv au))
            ((null au) (cons (+ av (* sign bv)) bu))
            ((null bu) (result bv au))
            ((and (%length-unit-p au) (%length-unit-p bu))
             (result (%convert bv bu au) au))
            (t (%fail btext))))))

(defun %multiply (a b btext)
  (destructuring-bind ((av . au) (bv . bu)) (list a b)
    (cond ((null au) (cons (* av bv) bu))
          ((null bu) (cons (* av bv) au))
          ((and (eq au :%) (eq bu :%)) (cons (/ (* av bv) 100) :%))
          ((eq au :%) (cons (* (/ av 100) bv) bu))
          ((eq bu :%) (cons (* av (/ bv 100)) au))
          (t (%fail btext)))))

(defun %divide (a b btext)
  (destructuring-bind ((av . au) (bv . bu)) (list a b)
    (when (zerop bv) (%fail btext))
    (cond ((null bu) (cons (/ av bv) au))
          ((null au) (%fail btext))
          ((and (eq au :%) (eq bu :%)) (cons (/ av bv) nil))
          ((eq bu :%)
           (if (%length-unit-p au) (cons (/ av (/ bv 100)) au) (%fail btext)))
          ((eq au :%) (%fail btext))
          (t (cons (/ (%convert av au :px) (%convert bv bu :px)) nil)))))

(defun %fold-arithmetic (op texts)
  "Left fold of OP (:+ :- :* :/) over the argument TEXTS -> result text."
  (let ((acc (%quantity (first texts))))
    (dolist (text (rest texts) (%quantity-text acc))
      (let ((q (%quantity text)))
        (setf acc (ecase op
                    (:+ (%add acc q text 1))
                    (:- (%add acc q text -1))
                    (:* (%multiply acc q text))
                    (:/ (%divide acc q text))))))))

(defun %ceil-text (text fraction)
  "CEILING of TEXT's number; with FRACTION a percent is ceiled as a fraction."
  (destructuring-bind (value . unit) (%quantity text)
    (%quantity-text
     (if (and fraction (eq unit :%))
         (cons (* 100 (ceiling (/ value 100))) unit)
         (cons (ceiling value) unit)))))
