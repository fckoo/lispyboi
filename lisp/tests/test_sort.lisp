(require "asserts")
(in-package :test-suite)

(assert-equal nil (sort-list nil #'<))
(assert-equal '(1) (sort-list '(1) #'<))
(assert-equal '(1 2) (sort-list '(1 2) #'<))
(assert-equal '(2 1) (sort-list '(1 2) #'>))

(let ((list '(1 3 5 7 9 10 8 6 4 2 0)))
  (assert-equal '(0 1 2 3 4 5 6 7 8 9 10)
                (sort-list list #'<))
  (assert-equal '(10 9 8 7 6 5 4 3 2 1 0)
                (sort-list list #'>)))


(assert-true (sortedp '() #'<))
(assert-true (sortedp '(1) #'<))
(assert-true (sortedp '(1 2) #'<))
(assert-true (sortedp '(1 2 3) #'<))
(assert-true (sortedp '(1 2 3 4) #'<))

(assert-false (sortedp '(1 2) #'>))
(assert-false (sortedp '(1 2 3) #'>))
(assert-false (sortedp '(1 2 3 4) #'>))

(let ((list)
      (predicate #'<))
  (dotimes (i 1000)
    (push (random) list))
  (setq list (sort-list! list predicate))
  (assert-true (sortedp list predicate)))
