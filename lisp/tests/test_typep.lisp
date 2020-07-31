(require "asserts")
(in-package :test-suite)


(assert-true (typep #\A 'character))
(assert-true (typep 123 'fixnum))
(assert-true (typep "test" 'string))
(assert-true (typep "test" 'array))
(assert-true (typep "test" '(simple-array)))
(assert-true (typep "test" '(simple-array character)))
(assert-true (typep "test" '(simple-array character 4)))
(assert-true (typep "test" '(simple-array * 4)))
(assert-true (typep "test" '(simple-array * *)))

(assert-false (typep "test" '(simple-array character 5)))
(assert-false (typep "test" '(simple-array * 3)))

(assert-true (typep (list 1 2 3) 'cons))
(assert-true (typep (list 1 2 3) 'list))
(assert-true (typep nil 'list))
(assert-false (typep nil 'cons))
