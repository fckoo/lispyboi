(require "asserts")


(defun foo (a) 123)
(defun bar (a) 456)


(let ((a 0) (b 0))
  (labels ((foo (a) (* a a))
           (bar (b) (+ b b)))
    (setf a (foo 30))
    (setf b (bar 30)))
  (assert-= 900 a)
  (assert-= 60 b))

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


(flet ((foo (x) (+ 10 (bar x)))
       (bar (x) (+ 10 (foo x)))
       (baz (x) (+ (foo x) (bar x))))
  (assert-= 466 (foo 123))
  (assert-= 133 (bar 456))
  (assert-= 579 (baz 999)))

(flet ((foo (a) (* a a))
       (bar (b) (+ b b)))
  (assert-= 100 (case 'quote
                  (quote (foo 10))
                  (t 999)))
  (assert-= 40 (case 'lambda
                 (lambda (bar 20))
                 (t 999))))
