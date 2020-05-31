(let ((foo 123)
      (bar (list 1 2 3 4))
      (baz 'qux))
  (assert-true (equal '(123 a b (1 2 3 4) c d 1 2 3 4 qux foo bar baz)
                      `(,foo a b ,bar c d ,@bar ,baz foo bar baz))))


(assert-eq nil `(,@(list)))

(assert-equal '(1 2 3) `(,@(list 1 2 3)))
