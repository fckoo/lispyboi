(require "asserts")
(in-package :test-suite)

(defgeneric test-generic (a))
(defmethod test-generic (a) (format nil "default ~s" a))
(defmethod test-generic ((a fixnum)) (format nil "fixnum ~s" a))
(defmethod test-generic ((a character)) (format nil "character ~s" a))
(defmethod test-generic ((a string)) (format nil "string ~s" a))
(defmethod test-generic ((a symbol)) (format nil "symbol ~s" a))


(assert-string= "fixnum 123" (test-generic 123))
(assert-string= "character #\\A" (test-generic #\A))
(assert-string= "string \"hello world\"" (test-generic "hello world"))
(assert-string= "symbol FOO" (test-generic 'foo))
(assert-string= "default (1 2 3)" (test-generic '(1 2 3)))


;; test automatic defgeneric when there was no prior defgeneric for the function
(defmethod this-method-does-not-exist ((a character) b)
  "character spec")

(defmethod this-method-does-not-exist ((a fixnum) b)
  "fixnum spec")


(assert-string= "character spec" (this-method-does-not-exist #\A 10))
(assert-string= "fixnum spec" (this-method-does-not-exist 65 11))
