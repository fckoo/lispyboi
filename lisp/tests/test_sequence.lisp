(require "asserts")
(in-package :test-suite)

(assert-equal nil (map1 #'identity nil))
(assert-equal '(1) (map1 #'identity '(1)))
(assert-equal '(1 2) (map1 #'identity '(1 2)))
(assert-equal '(1 2 3 4 5) (map1 #'identity '(1 2 3 4 5)))

(assert-equal '(1 4 9 16 25) (map1 (lambda (e) (* e e)) '(1 2 3 4 5)))



(assert-equal '((1 . 10))
              (map #'cons
                   '(1)
                   '(10)))

(assert-equal '((1 . 10) (2 . 20))
              (map #'cons
                   '(1 2)
                   '(10 20)))

(assert-equal '((1 . 10) (2 . 20) (3 . 30))
              (map #'cons
                   '(1 2 3)
                   '(10 20 30)))

(assert-equal '((1 10 100) (2 20 200) (3 30 300))
              (map #'list
                   '(1 2 3)
                   '(10 20 30)
                   '(100 200 300)))


(assert-equal '(1 3 5 7 9) (filter1 #'oddp '(0 1 2 3 4 5 6 7 8 9)))
(assert-equal '(0 2 4 6 8) (filter1 #'evenp '(0 1 2 3 4 5 6 7 8 9)))
