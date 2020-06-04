
(defmacro file-read-byte (file-stream) `(%file-read-byte ,file-stream))
(defun file-read-byte (file-stream) (file-read-byte file-stream))

(defmacro file-peek-byte (file-stream) `(%file-peek-byte ,file-stream))
(defun file-peek-byte (file-stream) (file-peek-byte file-stream))

(defmacro file-read-character (file-stream) `(%file-read-character ,file-stream))
(defun file-read-character (file-stream) (file-read-character file-stream))

(provide "file")
