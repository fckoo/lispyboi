(in-package :lispyboi)
(provide "random")


(defun split-mix-64 (x)
  (incf x 2177342782468422677)
  (setf x (* (bit-xor x (bit-shift x -30)) 4564476756301768121))
  (setf x (* (bit-xor x (bit-shift x -27)) 1499779743744070123))
  (bit-xor x (bit-shift x -31)))

(let ((high-bits (split-mix-64 12345678907777))
      (low-bits (split-mix-64 777777777777777)))
  (defun lehmer64 ()
    ;; This isn't true 128-bit multiplication because we don't have 64 bit
    ;; integers, unsigned integers, nor unsigned multiplcation. This is a
    ;; close enough approximation for a fast PRNG.
    (setf low-bits (* -2696494805208442699 low-bits))
    (setf high-bits (+ low-bits (* -2696494805208442699 high-bits))))

  (defun random-seed (seed)
    (setf high-bits (split-mix-64 seed))
    (setf low-bits (split-mix-64 (+ 1 seed)))))

(defun random ()
  (lehmer64))



(export '(random
          random-seed))
