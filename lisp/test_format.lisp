(require "asserts")
(require "format")

(assert-string= "" (format nil ""))

(assert-string= "-1" (format nil "~a" (- 1)))
(assert-string= "0" (format nil "~a" 0))
(assert-string= "1" (format nil "~a" 1))

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

(assert-string= "NIL" (format nil "~a" '()))
(assert-string= "(1)" (format nil "~a" '(1)))
(assert-string= "(1 2)" (format nil "~a" '(1 2)))
(assert-string= "(1 2 3)" (format nil "~a" '(1 2 3)))
(assert-string= "(1 2 3 4)" (format nil "~a" '(1 2 3 4)))
(assert-string= "(1 2 3 4 5)" (format nil "~a" '(1 2 3 4 5)))

(assert-string= "(1)" (format nil "~a" '(1)))
(assert-string= "(1 . 2)" (format nil "~a" '(1 . 2)))
(assert-string= "(1 2 . 3)" (format nil "~a" '(1 2 . 3)))
(assert-string= "(1 2 3 . 4)" (format nil "~a" '(1 2 3 . 4)))
(assert-string= "(1 2 3 4 . 5)" (format nil "~a" '(1 2 3 4 . 5)))

(assert-string= "(1 (((NIL))) 2)" (format nil "~a" '(1 (((()))) 2)))
