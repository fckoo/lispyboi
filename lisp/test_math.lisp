(require "asserts")

(setf +most-positive-fixnum+ 4611686018427387903)
(setf +most-negative-fixnum+ (- 4611686018427387904))

(assert-true (= (+) 0))
(assert-true (= (-) 0))
(assert-true (= (*) 1))

(assert-true (= 1 1))
(assert-true (= 111111 111111))
(assert-true (= 1234567890 1234567890))

(assert-true (= (+ 1 +most-positive-fixnum+) +most-negative-fixnum+))
(assert-true (= (- +most-negative-fixnum+ 1) +most-positive-fixnum+))
(assert-true (< +most-negative-fixnum+ 0))
(assert-true (< +most-negative-fixnum+ +most-positive-fixnum+))
(assert-true (> +most-positive-fixnum+ 0))

(assert-true (= 5 (+ 5)))
(assert-true (let ((a 5) (b 0)) (= a (+ 5 b))))
(assert-true (= 6 (+ 1 2 3)))

(assert-true (= 0 (- (+ 5 5) (+ 3 7))))

(assert-true (= 10 (* 5 2)))
(assert-true (= 240000 (* 10 20 30 40)))

(assert-true (< 1 2 3))
(assert-true (> 3 2 1))

(assert-true (<= 1 2 3))
(assert-true (<= 1 2 2 3))

(assert-true (>= 3 2 1))
(assert-true (>= 3 2 2 1))

