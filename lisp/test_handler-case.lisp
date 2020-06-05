(require "asserts")


(let ((val))
  (assert-true (= 10 (handler-case
                         (progn
                           (setf val 123)
                           (signal 'foo 10)
                           (setf val 456))
                       (foo (e) e))))
  (assert-true (= 123 val)))

(let ((val))
  (assert-true (= 100 (handler-case
                          (progn
                            (setf val 123)
                            (signal 'baz 10)
                            (setf val 456))
                        (foo (e) e)
                        (bar (e) (+ e e))
                        (baz (e) (* e e))
                        (qux (e) (cons e e)))))
  (assert-true (= 123 val)))

(let ((val))
  (assert-true (equal '(1 2 3 4) (handler-case
                                     (progn
                                       (setf val 123)
                                       (signal 'foo 1 2 3 4)
                                       (setf val 456))
                                   (foo (&rest args) 
                                     (setf val 999)
                                     args))))
  (assert-true (= 999 val)))


(let ((val))
  (assert-true (equal '(1 2 3 4) (handler-case
                                     (progn
                                       (setf val 123)
                                       (signal 'foo 1 2 3 4)
                                       (setf val 456))
                                   (t (&rest args) 
                                     (setf val 999)
                                     args)
                                   (foo (&rest args)
                                     (setf val 444)
                                     (reverse args)))))
  (assert-true (= 999 val)))


