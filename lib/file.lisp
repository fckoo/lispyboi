(provide "file")

(defmacro open (file-path direction) `(%open ,file-path ,direction))
(defun open (file-path direction) (open file-path direction))

(defmacro close (file-stream) `(%close ,file-stream))
(defun close (file-stream) (close file-stream))

(defmacro file-ok-p (file-stream) `(%file-ok-p ,file-stream))
(defun file-ok-p (file-stream) (file-ok-p file-stream))

(defmacro file-eof-p (file-stream) `(%file-eof-p ,file-stream))
(defun file-eof-p (file-stream) (file-eof-p file-stream))

(defmacro file-path (file-stream) `(%file-path ,file-stream))
(defun file-path (file-stream) (file-path file-stream))

(defmacro file-length (file-stream) `(%file-length ,file-stream))
(defun file-length (file-stream) (file-length file-stream))

(defmacro file-flush (file-stream) `(%file-flush ,file-stream))
(defun file-flush (file-stream) (file-flush file-stream))

(defun file-mode (file-stream)
  (case (%file-mode file-stream)
    (0 nil)
    (1 'read)
    (2 'overwrite)
    (3 '(read overwrite))
    (4 'append)
    (5 '(read append))))

(defun file-read-byte (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal eof-value)
          eof-value)
      (%file-read-byte file-stream)))

(defun file-peek-byte (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal eof-value)
          eof-value)
      (%file-peek-byte file-stream)))

(defun file-read-character (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal eof-value)
          eof-value)
      (%file-read-character file-stream)))

(defun file-peek-character (file-stream &optional eof-error-p eof-value)
  (if (file-eof-p file-stream)
      (if eof-error-p
          (signal eof-value)
          eof-value)
      (%file-peek-character file-stream)))

(defun file-read-line (file-stream)
  (let ((ss (make-string-stream)))
    (until (or (file-eof-p file-stream)
               (eql #\newline (code-char (file-peek-byte file-stream))))
           (string-stream-push ss (file-read-character file-stream)))
    (when (and (not file-eof-p file-stream)
               (eql #\newline (code-char (file-peek-byte file-stream))))
      (file-read-byte file-stream))
    (string-stream-str ss)))

(defmethod print-object ((fs file-stream) stream)
  (stream-puts stream "#S(FILE-STREAM '")
  (stream-puts stream (file-path fs))
  (stream-puts stream (if (file-ok-p fs) "' :OK T " " :OK NIL "))
  (stream-puts stream (if (file-eof-p fs) ":EOF T" ":EOF NIL"))
  (stream-puts stream ")"))

(defmethod stream-putchar ((stream file-stream) character)
  (%file-putchar stream character))

(defmethod stream-puts ((stream file-stream) string)
  (%file-puts stream string))

(defmethod stream-eof-p ((stream file-stream))
  (%file-eof-p stream))

(defmethod stream-peekc ((stream file-stream) &optional eof-error-p eof-value)
  (file-peek-character stream eof-error-p eof-value))

(defmethod stream-getc ((stream file-stream) &optional eof-error-p eof-value)
  (file-read-character stream eof-error-p eof-value))
