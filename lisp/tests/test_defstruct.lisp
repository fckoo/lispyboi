(require "asserts")
(in-package :test-suite)

(defstruct point (x 1) (y 2) (z 3) (w 4))

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

  (assert-string= "#S(POINT :X 42 :Y 420 :Z 3 :W 4)" (format nil "~s" pt))
  (assert-string= "#S(POINT :X 42 :Y 420 :Z 3 :W 4)" (format nil "~a" pt)))



(let ((pt (make-point)))
  (assert-= 1 (point-x pt))
  (assert-= 2 (point-y pt))
  (assert-= 3 (point-z pt))
  (assert-= 4 (point-w pt))

  (with-slots (x y z w)
      pt
    (assert-= 1 x)
    (assert-= 2 y)
    (assert-= 3 z)
    (assert-= 4 w)

    (setf x (+ x 10))
    (setf y (+ y 10))
    (setf z (+ z 10))
    (setf w (+ w 10))

    (assert-= 11 x)
    (assert-= 12 y)
    (assert-= 13 z)
    (assert-= 14 w))

  (assert-= 11 (point-x pt))
  (assert-= 12 (point-y pt))
  (assert-= 13 (point-z pt))
  (assert-= 14 (point-w pt)))
