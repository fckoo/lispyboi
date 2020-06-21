
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
  (assert-= 10000 (point-y pt)))
