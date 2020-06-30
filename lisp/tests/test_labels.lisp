(require "asserts")


(defun foo (a) 123)
(defun bar (a) 456)

(labels ((foo (a) (* a a))
         (bar (b) (* b 4)))

  (assert-= (* 123 123) (foo 123))

  (assert-= (* 123 4) (bar 123))

  (assert-= (* (* 123 123) 4) (bar (foo 123)))

  (assert-= (* (* 123 123) 4) (funcall #'bar (funcall #'foo 123))))


(labels ((foo (a) (bar a))
         (bar (b) (+ b b)))
  (assert-= 20 (case 'quote
                 (quote (foo 10))
                 (t 999)))
  (assert-= 40 (case 'lambda
                 (lambda (foo 20))
                 (t 999))))


(labels ((my-cons (a b) (cons a b))
         (my-car (cons) (car cons))
         (my-cdr (cons) (cdr cons)))
  (assert-eq 'foo (my-car (my-cons 'foo 'bar)))
  (assert-eq 'bar (my-cdr (my-cons 'foo 'bar))))
