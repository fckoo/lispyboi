(require "asserts")
(in-package :test-suite)

(assert-true (eq 'foo (car (list 'foo))))
(assert-true (eq 'bar (cdr (cons 'foo 'bar))))


(assert-true (eq 1 (first '(1 2 3 4 5))))
(assert-true (eq 2 (second '(1 2 3 4 5))))
(assert-true (eq 3 (third '(1 2 3 4 5))))
(assert-true (eq 4 (fourth '(1 2 3 4 5))))
(assert-true (eq 5 (fifth '(1 2 3 4 5))))


(assert-true (eq 'foo
                 (let ((list (list 1 2 3 4 5)))
                   (setf (car list) 'foo)
                   (car list))))

(assert-eq 'bar (let ((list (list 1 2 3 4 5)))
                  (setf (cdr list) 'bar)
                  (cdr list)))




(let ((test-list (list 10 11 12 13 14 15 16 17 18 19)))
  (assert-true (= 10 (length test-list)))
  (assert-true (= 15 (nth 5 test-list)))
  (assert-true (= 33 (setf (nth 3 test-list) 33)))
  (assert-true (= 33 (fourth test-list)))
  (assert-equal '(10 11 12 33 14 15 16 17 18 19) test-list)

  (assert-true (= 99 (setf (nth 5 test-list) 99)))
  (assert-equal '(10 11 12 33 14 99 16 17 18 19) test-list)
  (assert-true (= 99 (nth 5 test-list)))

  (assert-true (eq 'foo (setf (elt test-list 9) 'foo)))
  (assert-equal '(10 11 12 33 14 99 16 17 18 foo) test-list)
  (assert-equal (elt test-list 9) 'foo)

  (setf (cdr (cdddr test-list)) nil)
  (assert-equal '(10 11 12 33) test-list))



(let ((test-list (list 0 1 2 3 4)))
  (assert-equal (push 99 test-list) '(99 0 1 2 3 4))
  (assert-equal test-list '(99 0 1 2 3 4))
  (assert-equal 99 (pop test-list))
  (assert-equal test-list '(0 1 2 3 4))
  (assert-equal 0 (pop test-list))
  (assert-equal test-list '(1 2 3 4)))
