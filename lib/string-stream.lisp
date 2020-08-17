(in-package :lispyboi)
(provide "string-stream")

(defstruct string-stream
  (buffer (make-array 16 'character 0))
  (read-index 0))

(defun string-stream-length (string-stream)
  (array-length (string-stream-buffer string-stream)))

(defun string-stream-capacity (string-stream)
  (array-capacity (string-stream-buffer string-stream)))

(defun string-stream-write-char (string-stream character)
  (array-push-back (string-stream-buffer string-stream) character))

(defun string-stream-write-string (string-stream str)
  (dotimes (i (length str))
    (string-stream-write-char string-stream (aref str i))))

(defun string-stream-eof-p (string-stream)
  (>= (string-stream-read-index string-stream)
      (array-length (string-stream-buffer string-stream))))

(defun string-stream-peek-char (string-stream &optional eof-error-p eof-value)
  (if (string-stream-eof-p string-stream)
      (if eof-error-p
          (signal 'end-of-file)
          eof-value)
      (aref (string-stream-buffer string-stream)
            (string-stream-read-index string-stream))))

(defun string-stream-read-char (string-stream &optional eof-error-p eof-value)
  (let ((c (string-stream-peek-char string-stream eof-error-p eof-value)))
    (incf (string-stream-read-index string-stream))
    c))

(defun string-stream-empty-p (string-stream)
  (= 0 (string-stream-length string-stream)))

(defun string-stream-str (string-stream)
  (copy-array (string-stream-buffer string-stream)))

(defmacro with-input-from-string ((var string) &body body)
  `(let ((,var (make-string-stream)))
     (string-stream-write-string ,var ,string)
     ,@body))

(defmacro with-output-to-string ((var string) &body body)
  `(let ((,var (make-string-stream)))
     ,@(when string `((string-stream-write-string ,var ,string)))
     ,@body
     (string-stream-str ,var)))

(defmethod print-object ((ss string-stream) stream)
  (let ((len (string-stream-length ss))
        (buf (string-stream-buffer ss)))
    (dotimes (i len)
      (output-stream-write-char stream (aref buf i))))
  ss)

(defmethod output-stream-write-char ((ss string-stream) character)
  (string-stream-write-char ss character))

(defmethod output-stream-write-string ((ss string-stream) string)
  (string-stream-write-string ss string))

(defmethod input-stream-eof-p ((ss string-stream))
  (string-stream-eof-p ss))

(defmethod input-stream-peek-char ((ss string-stream) &optional eof-error-p eof-value)
  (string-stream-peek-char ss eof-error-p eof-value))

(defmethod input-stream-read-char ((ss string-stream) &optional eof-error-p eof-value)
  (string-stream-read-char ss eof-error-p eof-value))


(export '(string-stream
          make-string-stream
          string-stream-p
          string-stream-write-char
          string-stream-write-string
          string-stream-eof-p
          string-stream-peek-char
          string-stream-read-char
          string-stream-empty-p
          string-stream-str

          with-input-from-string
          with-output-to-string))
