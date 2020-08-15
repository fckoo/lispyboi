(require "asserts")
(in-package :test-suite)

(assert-string= "hello world" (string-downcase! "HeLlo WORLD"))
(assert-string= "HELLO WORLD" (string-upcase! "hello world"))

(assert-string= "foo" (string-trim "   foo      "))
(assert-string= "foo      " (string-trim-left "   foo      "))
(assert-string= "   foo" (string-trim-right "   foo      "))

(assert-string= "1, 2, 3, 4" (array-join ", " "1" "2" "3" "4"))

(assert-equal '("1" "2" "3" "4") (string-split "1 2 3 4"))
(assert-equal '("1" "2 3 4") (string-split "1 2 3 4" #\Space 1))
(assert-equal '("1" "2" "3 4") (string-split "1 2 3 4" #\Space 2))

(assert-equal '("1" "2 3 4") (string-split "1 2 3 4" " " 1))
(assert-equal '("1" "2" "3 4") (string-split "1 2 3 4" " " 2))

(assert-equal '("1" "2" "3" "4") (string-split "1;;2;;3;;4" ";;"))

(assert-equal '("1" "2" "3;;4") (string-split "1;;2;;3;;4" ";;" 2))


(assert-true (string= "foo" "foo"))
(assert-true (string= "foo" "oo" :start1 1))
(assert-true (string= "hello" "foo hello world" :start2 4 :end2 9))
(assert-true (string= "this is a test" "foo this is another test"
                      :start1 3 :start2 7
                      :end1 8 :end2 12))
