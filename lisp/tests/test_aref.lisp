(require "asserts")


(let ((array (make-array 10)))
  "Test array bounds checking"
  (assert-eq 'index-out-of-bounds-error
             (handler-case (progn
                             (setf (aref array (- 1)) #\a)
                             (signal 'failed))
               (t (sig &rest args)
                  sig)))
  (assert-eq 'index-out-of-bounds-error
             (handler-case (progn
                             (setf (aref array (length array)) #\a)
                             (signal 'failed))
               (t (sig &rest args)
                  sig)))
  (assert-eq 'passed
             (handler-case (progn
                             (setf (aref array 0) #\a)
                             (signal 'passed))
               (t (sig &rest args)
                  sig))))

(let ((array (make-array 10 'fixnum)))
  "Test array type checking"
  
  (assert-eq 'passed
             (handler-case (progn
                             (setf (aref array 0) 123)
                             (signal 'passed))
               (t (sig &rest args)
                  sig)))

  (assert-eq 'type-error
             (handler-case (progn
                             (setf (aref array 0) "test")
                             (signal 'failed))
               (t (sig &rest args)
                  sig))))
