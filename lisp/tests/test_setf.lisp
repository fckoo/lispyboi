(require "asserts")
(require "setf")


(let ((lst '(1 2 3 4 5)))
  (assert-= 10 (setf (car lst) 10))
  (assert-equal '(10 2 3 4 5) lst)

  (assert-= 20 (setf (cadr lst) 20))
  (assert-equal '(10 20 3 4 5) lst)

  (assert-= 30 (setf (caddr lst) 30))
  (assert-equal '(10 20 30 4 5) lst)

  (assert-= 40 (setf (cadddr lst) 40))
  (assert-equal '(10 20 30 40 5) lst)

  (assert-= 50 (setf (caddddr lst) 50))
  (assert-equal '(10 20 30 40 50) lst))

(let ((lst '(1 2 3 4 5)))
  (assert-= 10 (setf (first lst) 10))
  (assert-equal '(10 2 3 4 5) lst)

  (assert-= 20 (setf (second lst) 20))
  (assert-equal '(10 20 3 4 5) lst)

  (assert-= 30 (setf (third lst) 30))
  (assert-equal '(10 20 30 4 5) lst)

  (assert-= 40 (setf (fourth lst) 40))
  (assert-equal '(10 20 30 40 5) lst)

  (assert-= 50 (setf (fifth lst) 50))
  (assert-equal '(10 20 30 40 50) lst))
