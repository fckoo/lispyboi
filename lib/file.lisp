
(defmacro open (file-path direction) `(%open ,file-path ,direction))
(defun open (file-path direction) (open file-path direction))

(defmacro close (file-stream) `(%close ,file-stream))
(defun close (file-stream) (close file-stream))

(defmacro file-ok (file-stream) `(%file-ok ,file-stream))
(defun file-ok (file-stream) (file-ok file-stream))

(defmacro file-eof-p (file-stream) `(%file-eof-p ,file-stream))
(defun file-eof-p (file-stream) (file-eof-p file-stream))

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

(defmacro file-read-byte (file-stream) `(%file-read-byte ,file-stream))
(defun file-read-byte (file-stream) (file-read-byte file-stream))

(defmacro file-peek-byte (file-stream) `(%file-peek-byte ,file-stream))
(defun file-peek-byte (file-stream) (file-peek-byte file-stream))

(defmacro file-read-character (file-stream) `(%file-read-character ,file-stream))
(defun file-read-character (file-stream) (file-read-character file-stream))

(provide "file")
