(in-package :lispyboi)
(provide "file")

(defmacro open (file-path direction) `(kernel::%open ,file-path ,direction))
(defun open (file-path direction) (open file-path direction))

(defmacro close (file-stream) `(kernel::%close ,file-stream))
(defun close (file-stream) (close file-stream))

(defmacro file-ok-p (file-stream) `(kernel::%file-ok-p ,file-stream))
(defun file-ok-p (file-stream) (file-ok-p file-stream))

(defmacro file-eof-p (file-stream) `(kernel::%file-eof-p ,file-stream))
(defun file-eof-p (file-stream) (file-eof-p file-stream))

(defmacro file-path (file-stream) `(kernel::%file-path ,file-stream))
(defun file-path (file-stream) (file-path file-stream))

(defmacro file-flush (file-stream) `(kernel::%file-flush ,file-stream))
(defun file-flush (file-stream) (file-flush file-stream))

(defun file-mode (file-stream)
  (case (kernel::%file-mode file-stream)
    (0 nil)
    (1 'read)
    (2 'overwrite)
    (3 '(read overwrite))
    (4 'append)
    (5 '(read append))))

(defun file-read-byte (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal 'end-of-file)
          eof-value)
      (kernel::%file-read-byte file-stream)))

(defun file-peek-byte (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal 'end-of-file)
          eof-value)
      (kernel::%file-peek-byte file-stream)))

(defun file-read-character (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal 'end-of-file)
          eof-value)
      (kernel::%file-read-character file-stream)))

(defun file-peek-character (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal 'end-of-file)
          eof-value)
      (kernel::%file-peek-character file-stream)))

(defun file-read-line (file-stream)
  (with-output-to-string (ss)
    (until (or (file-eof-p file-stream)
               (eql #\newline (code-char (file-peek-byte file-stream))))
           (string-stream-write-char ss (file-read-character file-stream)))
    (when (and (not (file-eof-p file-stream))
               (eql #\newline (code-char (file-peek-byte file-stream))))
      (file-read-byte file-stream))))

(defmethod print-object ((fs file-stream) stream)
  (output-stream-write-string stream "#S(FILE-STREAM '")
  (output-stream-write-string stream (file-path fs))
  (output-stream-write-string stream (if (file-ok-p fs) "' :OK T " " :OK NIL "))
  (output-stream-write-string stream (if (file-eof-p fs) ":EOF T" ":EOF NIL"))
  (output-stream-write-string stream ")")
  fs)

(defmethod output-stream-write-char ((stream file-stream) character)
  (kernel::%file-putchar stream character))

(defmethod output-stream-write-string ((stream file-stream) string)
  (kernel::%file-puts stream string))

(defmethod input-stream-eof-p ((stream file-stream))
  (kernel::%file-eof-p stream))

(defmethod input-stream-peek-char ((stream file-stream) &optional eof-error-p eof-value)
  (file-peek-character stream eof-error-p eof-value))

(defmethod input-stream-read-char ((stream file-stream) &optional eof-error-p eof-value)
  (file-read-character stream eof-error-p eof-value))

(export '(open
          close
          file-ok-p
          file-eof-p
          file-path
          file-flush
          file-mode
          file-read-byte
          file-peek-byte
          file-read-character
          file-peek-character
          file-read-line

          read
          overwrite
          append))
