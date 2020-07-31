(require "asserts")
(in-package :test-suite)

(let ((val 0))
  (ignore-errors
   (unwind-protect
        (unwind-protect
             (progn (setf val 100)
                    (signal 'foo-bar "heheheh")
                    (setf val 200))
          (incf val)
          (incf val)
          (incf val))
     (incf val)
     (incf val)
     (incf val)))
  (assert-= 106 val))
