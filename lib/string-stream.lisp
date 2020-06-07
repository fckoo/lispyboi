
(require "setf")

(defun make-string-stream ()
  (let ((length 16))
    (list 0 (make-array length 'character))))

(defun %string-copy (destination source n)
  (dotimes (i n)
    (setf (aref destination i) (aref source i))))

(defun %copy-expand-string (str)
  (let ((new-str (make-array (* 2 (length str)) 'character)))
    (%string-copy new-str str (length str))
    new-str))

(defun string-stream-length (string-stream)
  (first string-stream))

(defun set-string-stream-length (string-stream new-value)
  (setf  (first string-stream) new-value))

(defsetf string-stream-length set-string-stream-length)

(defun string-stream-buffer (string-stream)
  (second string-stream))

(defun set-string-stream-buffer (string-stream new-value)
  (setf (second string-stream) new-value))

(defsetf string-stream-buffer set-string-stream-buffer)

(defun string-stream-push (string-stream character)
  (let ((len (string-stream-length string-stream))
        (str (string-stream-buffer string-stream)))
    (when (= len (length str))
      (setf str (setf (string-stream-buffer string-stream) (%copy-expand-string str))))
    (setf (aref str len) character)
    (incf (string-stream-length string-stream))
    string-stream))

(defun string-stream-append (string-stream str)
  (dotimes (i (length str))
    (string-stream-push string-stream (aref str i))))

(defun string-stream-str (string-stream)
  (let ((string (make-array (string-stream-length string-stream) 'character)))
    (%string-copy string (string-stream-buffer string-stream) (string-stream-length string-stream))
    string))

(provide "string-stream")
