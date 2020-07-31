(require "asserts")
(in-package :test-suite)

(assert-= (- 1) -1)
(assert-= 1 +1)

(assert-= (- 1234) -1234)
(assert-= 1234 +1234)


(assert-= 0 #b0)
(assert-= 1 #b1)
(assert-= 2 #b10)
(assert-= 3 #b11)
(assert-= 255 #b11111111)

(assert-= 0 #x0)
(assert-= 1 #x1)
(assert-= 16 #x10)
(assert-= 17 #x11)
(assert-= 255 #xff)

(assert-= 0 #o0)
(assert-= 1 #o1)
(assert-= 8 #o10)
(assert-= 9 #o11)
(assert-= 255 #o377)
