(require "asserts")


(let ((foo '(1 2 3 4 5 6)))
  (destructuring-bind (a b c)
      foo
    (assert-= 1 a)
    (assert-= 2 b)
    (assert-= 3 c)))

(let ((foo '(1 2 3 4 5 6)))
  (destructuring-bind (a b c . rest)
      foo
    (assert-= 1 a)
    (assert-= 2 b)
    (assert-= 3 c)
    (assert-equal '(4 5 6) rest)))

(let ((foo '(1 2 (3 4) 5 6)))
  (destructuring-bind (a b (c1 c2) . rest)
      foo
    (assert-= 1 a)
    (assert-= 2 b)
    (assert-= 3 c1)
    (assert-= 4 c2)
    (assert-equal '(5 6) rest)))

(let ((foo '(1 2 (3 4 5 6 7) 8 9 10)))
  (destructuring-bind (a b (c1 c2 . c-rest) . rest)
      foo
    (assert-= 1 a)
    (assert-= 2 b)
    (assert-= 3 c1)
    (assert-= 4 c2)
    (assert-equal '(5 6 7) c-rest)
    (assert-equal '(8 9 10) rest)))
