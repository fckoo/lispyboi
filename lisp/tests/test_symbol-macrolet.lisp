(require "asserts")

(defstruct test-thing (val 456))

(let ((foo 123)
      (thing (make-test-thing)))
  (assert-= 123 foo)
  (assert-= 456 (test-thing-val thing))
  (symbol-macrolet ((x foo)
                    (val (slot-value thing 'val)))
    (setf x 999)
    (setf val 'ok))
  (assert-= 999 foo)
  (assert-eq 'ok (test-thing-val thing)))
