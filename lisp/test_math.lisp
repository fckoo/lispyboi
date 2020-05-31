
(setf +most-positive-fixnum+ 4611686018427387903)
(setf +most-negative-fixnum+ (- 4611686018427387904))

(assert-true (eq 1 1))
(assert-true (eq 111111 111111))
(assert-true (eq 1234567890 1234567890))

(assert-true (eq (+ 1 +most-positive-fixnum+) +most-negative-fixnum+))
(assert-true (eq (- +most-negative-fixnum+ 1) +most-positive-fixnum+))
(assert-true (< +most-negative-fixnum+ 0))
(assert-true (< +most-negative-fixnum+ +most-positive-fixnum+))

(assert-true (eq 5 (+ 5)))
(assert-true (let ((a 5) (b 0)) (eq a (+ 5 b))))
(assert-true (eq 6 (+ 1 2 3)))

(assert-true (eq 0 (- (+ 5 5) (+ 3 7))))

(assert-true (eq 10 (* 5 2)))
(assert-true (eq 240000 (* 10 20 30 40)))
