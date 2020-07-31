(require "asserts")
(in-package :test-suite)

(let ((hash-table (make-hash-table)))
  (assert-eq nil (gethash 'foo hash-table))
  (assert-= 123 (gethash 'foo hash-table 123))

  (assert-= 999 (setf (gethash 'foo hash-table) 999))
  (assert-eq 'lol (setf (gethash 'bar hash-table) 'lol))
  (assert-string= "yep" (setf (gethash 'baz hash-table) "yep"))

  (assert-= 999 (gethash 'foo hash-table))
  (assert-eq 'lol (gethash 'bar hash-table))
  (assert-string= "yep" (gethash 'baz hash-table)))
