(provide "print-object")

(require "stream")
(require "math")

(defgeneric print-object (object stream)
  "Write a printable representation of OBJECT to STREAM.

OBJECT should be returned")

(defmethod print-object ((obj (simple-array * *)) stream)
  (stream-puts stream "#(")
  (when (/= 0 (length obj))
    (dotimes (i (- (length obj) 1))
      (print-object (aref obj i) stream)
      (stream-putchar stream #\Space))
    (print-object (aref obj (- (length obj) 1)) stream))
  (stream-puts stream ")")
  obj)

(defmethod print-object ((obj (simple-array bit *)) stream)
  (stream-puts stream "#(BIT-VECTOR ")
  (dotimes (i (length obj))
    (stream-putchar stream (if (= 1 (aref obj (- (length obj) 1 i))) #\1 #\0)))
  (stream-puts stream ")")
  obj)

(defmethod print-object ((o string) stream)
  (stream-putchar stream #\")
  (stream-puts stream o)
  (stream-putchar stream #\")
  o)

(defmethod print-object ((o null) stream)
  (stream-puts stream "NIL")
  nil)

(defmethod print-object ((c character) stream)
  (case c
    (#\Space (stream-puts stream "#\\Space"))
    (#\Newline (stream-puts stream "#\\Newline"))
    (#\Tab (stream-puts stream "#\\Tab"))
    (#\Return (stream-puts stream "#\\Return"))
    (t
     (stream-putchar stream #\#)
     (stream-putchar stream #\\)
     (stream-putchar stream c)))
  c)

(defmethod print-object ((o symbol) stream)
  (stream-puts stream (symbol-name o))
  o)

(defmethod print-object ((n fixnum) stream)
  (let ((number n))
    (if (= number 0)
        (stream-putchar stream #\0)
        (progn
          (when (< number 0)
            (setf number (- number))
            (stream-putchar stream #\-))
          (let ((chars))
            (while (/= 0 number)
              (push (code-char (+ (char-code #\0) (rem number 10))) chars)
              (setf number (floor number 10)))
            (dolist (c chars)
              (stream-putchar stream c))))))
  n)

(defmethod print-object ((o cons) stream)
  (stream-putchar stream #\()
  (let ((cur o))
    (while (consp (cdr cur))
           (print-object (car cur) stream)
           (stream-putchar stream #\Space)
           (setf cur (cdr cur)))
    (cond ((null (cdr cur))
           (print-object (car cur) stream))
          (t
           (print-object (car cur) stream)
           (stream-puts stream " . ")
           (print-object (cdr cur) stream))))
  (stream-putchar stream #\))
  o)
