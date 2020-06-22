(provide "string-stream")
(require "setf")
(require "defstruct")

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
