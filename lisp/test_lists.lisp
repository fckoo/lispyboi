

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
