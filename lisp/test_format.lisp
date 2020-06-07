(require "asserts")
(require "format")

(assert-string= "" (format nil ""))

(assert-string= "123" (format nil "123"))
(assert-string= "123" (format nil "~A" 123))
(assert-string= "123" (format nil "~A" "123"))

(assert-string= "~" (format nil "~~"))
(assert-string= "~~" (format nil "~~~~"))
(assert-string= "~~~" (format nil "~~~~~~"))

(assert-string= (make-string #\newline) (format nil "~%"))
(assert-string= (make-string #\newline #\newline) (format nil "~%~%"))

(assert-string= "test 123 TEST" (format nil "~a ~a ~a" (make-symbol "test") 123 'test))

(assert-string= "hello" (format nil "h~a~a~ao" #\e #\l #\l))
