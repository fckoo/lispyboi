(require "asserts")
(in-package :test-suite)

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

(assert-= 10 (floor 10))
(assert-= 1 (floor 10 10))
(assert-= 10 (floor 100 10))

(assert-= 1 (rem 1 5))
(assert-= 2 (rem 2 5))
(assert-= 3 (rem 3 5))
(assert-= 4 (rem 4 5))
(assert-= 0 (rem 5 5))
(assert-= 1 (rem 6 5))

(assert-= 42 (min 42 42))
(assert-= 42 (min 42 123))
(assert-= 42 (min 123 42))

(assert-= 123 (max 123 123))
(assert-= 123 (max 42 123))
(assert-= 123 (max 123 42))

(assert-= 123 (abs (- 123)))
(assert-= 123 (abs 123))

(assert-= 126 (bit-shift 63 1))
(assert-= 31 (bit-shift 63 -1))

(assert-= 0 (bit-and #xFFFF0000 #x0000FFFF))
(assert-= #b1111 (bit-ior #b1100 #b0011))
(assert-= #b1111 (bit-xor #b1010 #b0101))
(assert-= -1 (bit-not 0))
(assert-= -2 (bit-not 1))
(assert-= +most-positive-fixnum+ (bit-not +most-negative-fixnum+))
(assert-= +most-negative-fixnum+ (bit-not +most-positive-fixnum+))
