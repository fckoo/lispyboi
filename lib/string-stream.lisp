(provide "string-stream")

(defstruct string-stream
  (length 0)
  (index 0)
  (buffer (make-array 16 'character)))

(defun string-copy (destination source n)
  (dotimes (i n)
    (setf (aref destination i) (aref source i))))

(defun string-stream-push (string-stream character)
  (labels ((copy-expand-string (str)
             (let ((new-str (make-array (* 2 (length str)) 'character)))
               (string-copy new-str str (length str))
               new-str)))
    (let ((len (string-stream-length string-stream))
          (str (string-stream-buffer string-stream)))
      (when (= len (length str))
        (setf str (setf (string-stream-buffer string-stream)
                        (copy-expand-string str))))
      (setf (aref str len) character)
      (incf (string-stream-length string-stream))
      string-stream)))

(defun string-stream-append (string-stream str)
  (dotimes (i (length str))
    (string-stream-push string-stream (aref str i))))

(defun string-stream-eof-p (string-stream)
  (= (string-stream-index string-stream)
     (string-stream-length string-stream)))

(defun string-stream-peekc (string-stream &optional eof-error-p eof-value-p)
  (if (>= (string-stream-index string-stream)
          (string-stream-length string-stream))
      (if eof-error-p
          (signal 'end-of-file)
          eof-value)
      (aref (string-stream-buffer string-stream)
            (string-stream-index string-stream))))

(defun string-stream-getc (string-stream &optional eof-error-p eof-value-p)
  (let ((c (string-stream-peekc string-stream eof-error-p eof-value-p)))
    (incf (string-stream-index string-stream))
    c))

(defun string-stream-empty-p (string-stream)
  (= 0 (string-stream-length string-stream)))

(defun string-stream-str (string-stream)
  (let ((string (make-array (string-stream-length string-stream) 'character)))
    (string-copy string
                 (string-stream-buffer string-stream)
                 (string-stream-length string-stream))
    string))

(defmacro with-input-from-string ((var string) &body body)
  `(let ((,var (make-string-stream)))
     (string-stream-append ,var ,string)
     ,@body))

(defmethod print-object ((ss string-stream) stream)
  (let ((len (string-stream-length ss))
        (buf (string-stream-buffer ss)))
    (dotimes (i len)
      (stream-putchar stream (aref buf i))))
  ss)

(defmethod stream-putchar ((ss string-stream) character)
  (string-stream-push ss character))

(defmethod stream-puts ((ss string-stream) string)
  (string-stream-append ss string))

(defmethod stream-eof-p ((ss string-stream))
  (string-stream-eof-p ss))

(defmethod stream-peekc ((ss string-stream) &optional eof-error-p eof-value)
  (string-stream-peekc ss eof-error-p eof-value))

(defmethod stream-getc ((ss string-stream) &optional eof-error-p eof-value)
  (string-stream-getc ss eof-error-p eof-value))
