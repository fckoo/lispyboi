(require "defgeneric")
(require "asserts")
(require "format")

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
