
(require "asserts")
(require "defstruct")

(defstruct point (x) (y))

(let ((pt (make-point 123 456)))
  (assert-eq 'point (type-of pt))
  (assert-= 123 (point-x pt))
  (assert-= 456 (point-y pt))

  (assert-= 999 (setf (point-x pt) 999))
  (assert-= 10000 (setf (point-y pt) 10000))

  (assert-= 999 (point-x pt))
  (assert-= 10000 (point-y pt))


  (assert-= 999 (slot-value pt 'x))
  (assert-= 10000 (slot-value pt 'y))

  (assert-= 42 (setf (slot-value pt 'x) 42))
  (assert-= 420 (setf (slot-value pt 'y) 420))

  (assert-= 42 (slot-value pt 'x))
  (assert-= 420 (slot-value pt 'y))

  (assert-string= "#S(POINT :X 42 :Y 420)" (format nil "~s" pt))
  (assert-string= "#S(POINT :X 42 :Y 420)" (format nil "~a" pt))
  )
