(require "asserts")

(let ((val))
  (assert-= 10 (handler-case
                   (progn
                     (setf val 123)
                     (signal 'foo 10)
                     (setf val 456))
                 (foo (e) e)))
  (assert-= 123 val))

(let ((val))
  (assert-= 100 (handler-case
                    (progn
                      (setf val 123)
                      (signal 'baz 10)
                      (setf val 456))
                  (foo (e) e)
                  (bar (e) (+ e e))
                  (baz (e) (* e e))
                  (qux (e) (cons e e))))
  (assert-= 123 val))

(let ((val))
  (assert-equal '(1 2 3 4)
                (handler-case
                    (progn
                      (setf val 123)
                      (signal 'foo 1 2 3 4)
                      (setf val 456))
                  (foo (&rest args) 
                    (setf val 999)
                    args)))
  (assert-= 999 val))


(let ((val))
  (assert-equal '(foo 10 20 30 40)
                (handler-case
                    (progn
                      (setf val 123)
                      (signal 'foo 10 20 30 40)
                      (setf val 456))
                  (t (&rest args) 
                    (setf val 999)
                    args)
                  (foo (&rest args)
                    (setf val 444)
                    (reverse args))))
  (assert-= 999 val))


(let ((val 0))
  (handler-case
      (handler-case (signal 'foo)
        (bar ()
          (setf val 333)))
    (foo ()
      (setf val 4545)))
  (assert-= 4545 val))

(let ((val 0))
  (handler-case
      (handler-case (signal 'foo)
        (t (sig)
          (setf val 333)
          (signal sig)))
    (foo ()
      (setf val 4545)))
  (assert-= 4545 val))

(let ((val 0))
  (handler-case
      (handler-case
          (progn (handler-case (signal 'g)
                   (g () (setf val 7)))
                 (setf val 123))
        (h () (setf val 8)))
    (foo ()
      (setf val 4545)))
  (assert-= 123 val))

(let ((val 0))
  (handler-case
      (handler-case
          (handler-case
              (handler-case
                  (handler-case
                      (handler-case
                          (handler-case
                              (handler-case
                                  (handler-case (signal 'foo)
                                    (a () (setf val 1)))
                                (b () (setf val 2)))
                            (c () (setf val 3)))
                        (d () (setf val 4)))
                    (e () (setf val 5)))
                (f () (setf val 6)))
            (g () (setf val 7)))
        (h () (setf val 8)))
    (foo ()
      (setf val 4545)))
  (assert-= 4545 val))

(let ((val 0))
  (handler-case
      (handler-case
          (handler-case
              (handler-case
                  (handler-case
                      (handler-case
                          (handler-case
                              (handler-case
                                  (handler-case (signal 'd)
                                    (a () (setf val 1)))
                                (b () (setf val 2)))
                            (c () (setf val 3)))
                        (d () (setf val 4)))
                    (e () (setf val 5)))
                (f () (setf val 6)))
            (g () (setf val 7)))
        (h () (setf val 8)))
    (foo ()
      (setf val 4545)))
  (assert-= 4 val))

(let ((val 0))
  (handler-case
      (handler-case
          (handler-case
              (handler-case
                  (handler-case
                      (handler-case
                          (handler-case
                              (handler-case
                                  (handler-case (signal 'a)
                                    (a () (setf val 1)))
                                (b () (setf val 2)))
                            (c () (setf val 3)))
                        (d () (setf val 4)))
                    (e () (setf val 5)))
                (f () (setf val 6)))
            (g () (setf val 7)))
        (h () (setf val 8)))
    (foo ()
      (setf val 4545)))
  (assert-= 1 val))


(let ((val 0))
  (handler-case
      (handler-case (signal 'foo)
        (t (sig)
          (setf val 333)
          (signal 'bar)))
    (bar ()
      (setf val 4545)))
  (assert-= 4545 val))

(let ((val 0))
  (handler-case
      (handler-case (signal 'foo)
        (t (sig)
          (error 5555)))
    (error (n)
      (setf val n)))
  (assert-= 5555 val))
