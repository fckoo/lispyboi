(require "asserts")
(in-package :test-suite)

(assert-eq 'foo (if t 'foo 'bar))
(assert-eq 'bar (if nil 'foo 'bar))
(assert-= 123 (if t 123))
(assert-eq nil (if nil 123))

(defun if-constant-true (a b)
  (if :true a b))

(defun if-constant-false (a b)
  (if nil a b))

(defun if-test (test a b)
  (if test a b))

(assert-eq 'foo (if-constant-true 'foo 'bar))
(assert-eq 'bar (if-constant-false 'foo 'bar))
(assert-eq 'foo (if-test t 'foo 'bar))
(assert-eq 'bar (if-test nil 'foo 'bar))
