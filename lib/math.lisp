
(setf +most-positive-fixnum+ 4611686018427387903)
(setf +most-negative-fixnum+ (- 4611686018427387904))

(defun floor (n &optional d)
  (if d
      (/ n d)
      n))

(defun rem (n d)
  (- n (* d (floor n d))))


(defun max (a b) (if (> a b) a b))

(defun min (a b) (if (< a b) a b))

(defun abs (a) (if (< a 0) (- a) a))

(provide "math")
