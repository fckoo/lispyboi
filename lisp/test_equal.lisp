(assert-false (equal 'a 'b))
(assert-true (equal 'a 'a))
(assert-true (equal 3 3))

(assert-true (equal 1 1))
(assert-false (equal 1 2))
(assert-false (equal 2 1))

(assert-true (equal #\space #\space))
(assert-false (equal #\a #\b))
(assert-false (equal #\b #\a))

(assert-true (equal #\A #\A))
(assert-false (equal #\A #\a))

(assert-true (equal '(1 2 3 4 5) '(1 2 3 4 5)))

;;                     v            v
(assert-false (equal '(9 2 3 4 5) '(1 2 3 4 5)))

(assert-true (equal (cons 1 2) (cons 1 2)))

;;                         v          v
(assert-false (equal (cons 1 2) (cons 2 2)))

(assert-true (equal (cons 1 (cons 2 (cons 3 nil)))
                    (list 1 2 3)))

(assert-true (equal '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)
                    '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)))

(assert-false (equal '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)
                     ;;               v
                     '(1 (2 3 (4 5 6 (8 . 42) 8) 9) 10)))

;;                                        vv
(assert-false (equal '(1 (2 3 (4 5 6 (7 . 41) 8) 9) 10)
                     '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)))
