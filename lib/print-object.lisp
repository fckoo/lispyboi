(provide "print-object")

(require "stream")
(require "math")

(defgeneric print-object (object stream)
  "Write a printable representation of OBJECT to STREAM.

OBJECT should be returned")

(defmethod print-object ((o string) stream)
  (stream-puts stream o)
  o)

(defmethod print-object ((o null) stream)
  (stream-puts stream "NIL")
  nil)

(defmethod print-object ((o symbol) stream)
  (stream-puts stream (symbol-name o))
  o)

(defmethod print-object ((n fixnum) stream)
  (let ((number n))
    (if (= number 0)
        (stream-putchar stream #\0)
        (if (< 0 number)
            (stream-putchar stream #\-)
            (let ((chars))
              (while (/= 0 number)
                (push (code-char (+ (char-code #\0) (rem number 10))) chars)
                (setf number (floor number 10)))
              (dolist (c (reverse chars))
                (stream-putchar stream c))))))
  n)

(defmethod print-object ((o cons) stream)
  (stream-putchar #\()
  (let ((cur o))
    (while (consp (cdr cur))
      (print-object (car cur) stream)
      (stream-putchar #\Space)
      (setf cur (cdr cur)))
    (cond ((null (cdr cur))
           (print-object (car cur) stream))
          (t
           (print-object (car cur) stream)
           (stream-puts " . ")
           (print-object (cdr cur) stream))))
  (stream-putchar #\))
  o)
