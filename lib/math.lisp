(in-package :lispyboi)
(provide "math")

(setf +most-positive-fixnum+ 4611686018427387903)
(setf +most-negative-fixnum+ (- 4611686018427387904))

(defun floor (n &optional d)
  (if d
      (/ n d)
      n))

(defun rem (n d)
  (- n (* d (floor n d))))

(defun min (&rest nums)
  (let ((n +most-positive-fixnum+))
    (dolist (e nums)
      (when (< e n)
        (setq n e)))
    n))

(defun max (&rest nums)
  (let ((n +most-negative-fixnum+))
    (dolist (e nums)
      (when (> e n)
        (setq n e)))
    n))

(defun abs (a) (if (< a 0) (- a) a))

(defun evenp (n) (= 0 (rem n 2)))
(defun oddp (n) (/= 0 (rem n 2)))

(export '(+most-positive-fixnum+
          +most-negative-fixnum+
          floor
          rem
          max
          min
          abs
          evenp
          oddp))
