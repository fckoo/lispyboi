
(require "math")
(require "string-stream")

(defun reverse-string (string)
  (let ((new-str (make-array (length string) 'character)))
    (dotimes (i (length new-str))
      (setf (aref new-str i) (aref string (- (length string) i 1))))
    new-str))

(defun %format-fixnum (number)
  (let ((ss (make-string-stream))
        (neg (< number 0)))
    (setf number (abs number))
    (while (/= number 0)
           (string-stream-push ss (code-char (+ 48 (rem number 10))))
           (setf number (floor number 10)))
    (when neg
      (string-stream-push ss #\-))
    (reverse-string (string-stream-str ss))))

(defun %format-cons (cons)
  "TODO")

(defun copy-string (string)
  (let ((str (make-array (length string) 'character)))
    (dotimes (i (length str))
      (setf (aref str i) (aref string i)))
    str))

(defun %format-impl (format args)
  (let ((ss (make-string-stream))
        (i 0)
        (end (length format)))
    (labels ((peek (i) (and (< i (length format))
                            (aref format i))))
      (while (< i end)
             (if (eql #\~ (aref format i))
                 (let ((spec (peek (+ 1 i))))
                   (cond
                     ((or (eql #\A spec) (eql #\a spec))
                      (let ((arg (pop args)))
                        (incf i 1)
                        (string-stream-append ss
                                              (cond ((stringp arg) arg)
                                                    ((symbolp arg) (symbol-name arg))
                                                    ((fixnump arg) (%format-fixnum arg))
                                                    ((characterp arg) (make-string arg))
                                                    ((null arg) "NIL")
                                                    ((consp arg) (%format-cons))
                                                    (t "??")))))
                     ((eql #\~ spec)
                      (incf i 1)
                      (string-stream-push ss #\~))
                     ((eql #\% spec)
                      (incf i 1)
                      (string-stream-push ss #\Newline))))
                 (string-stream-push ss (aref format i))) 
             (incf i)))
    (string-stream-str ss)))

(defun format (stream format &rest args)
  "Formats ARGS based on the FORMAT specificer and prints to STREAM.

Format specifiers begin with the ~ character and include:
    ~A and ~a : the aesthetic representation of the argument
    ~~        : the literal character #\~
    ~%        : the literal character #\Newline

If STREAM is NIL, the formatted string is returned instead of being
printed.

If STREAM is T, *STANDARD-OUTPUT* is used as the output stream and
NIL is returned."
  (let ((formatted (%format-impl format args)))
    (when (eq t stream)
      (setf stream *STANDARD-OUTPUT*))
    (if stream
        (and (print formatted stream) nil)
        formatted)))

(provide "format")
