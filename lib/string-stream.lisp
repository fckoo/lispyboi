(provide "string-stream")

(defstruct string-stream
  (length 0)
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

(defun string-stream-str (string-stream)
  (let ((string (make-array (string-stream-length string-stream) 'character)))
    (string-copy string (string-stream-buffer string-stream) (string-stream-length string-stream))
    string))


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
