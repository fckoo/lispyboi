(require "asserts")


(defun foo (a) 123)
(defun bar (a) 456)

(labels ((foo (a) (* a a))
         (bar (b) (* b 4)))

  (assert-= (* 123 123) (foo 123))

  (assert-= (* 123 4) (bar 123))

  (assert-= (* (* 123 123) 4) (bar (foo 123)))

  (assert-= (* (* 123 123) 4) (funcall #'bar (funcall #'foo 123))))
