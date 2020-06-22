(provide "format")
(require "math")
(require "string-stream")

(defun reverse-string (string)
  (let ((new-str (make-array (length string) 'character)))
    (dotimes (i (length new-str))
      (setf (aref new-str i) (aref string (- (length string) i 1))))
    new-str))

(defun %format-fixnum (number &optional (radix 10))
  (if (= number 0)
      (make-string #\0)
      (let ((ss (make-string-stream))
            (neg (< number 0)))
        (setf number (abs number))
        (while (/= number 0)
          (let ((rem (rem number radix)))
            (if (> rem 10)
                (string-stream-push ss (code-char (+ (char-code #\A) (- rem 10))))
                (string-stream-push ss (code-char (+ (char-code #\0) rem)))))
          (setf number (floor number radix)))
        (when neg
          (string-stream-push ss #\-))
        (reverse-string (string-stream-str ss)))))

(defun %format-cons (cons format-object-func)
  (let ((ss (make-string-stream)))
    (string-stream-push ss #\()
    (while (consp (cdr cons))
      (string-stream-append ss (funcall format-object-func (car cons)))
      (string-stream-push ss #\Space)
      (setf cons (cdr cons)))
    (cond ((null (cdr cons))
           (string-stream-append ss (funcall format-object-func (car cons))))
          (t
           (string-stream-append ss (funcall format-object-func (car cons)))
           (string-stream-append ss " . ")
           (string-stream-append ss (funcall format-object-func (cdr cons)))))
    (string-stream-push ss #\))
    (string-stream-str ss)))

(defun copy-string (string)
  (let ((str (make-array (length string) 'character)))
    (dotimes (i (length str))
      (setf (aref str i) (aref string i)))
    str))

(defun %format-object-a (object)
  (cond ((stringp object) object)
        ((symbolp object) (symbol-name object))
        ((fixnump object) (%format-fixnum object))
        ((characterp object) (make-string object))
        ((null object) "NIL")
        ((consp object) (%format-cons object #'%format-object-a))
        (t "??")))

(defun %format-escape-string (string)
  (let ((ss (make-string-stream)))
    (string-stream-push ss #\")
    (dotimes (i (length string))
      (let ((c (aref string i)))
        (case c
          (#\"
           (string-stream-push ss #\\)
           (string-stream-push ss #\"))
          (t
           (string-stream-push ss c)))))
    (string-stream-push ss #\")
    (string-stream-str ss)))

(defun %format-object-s (object)
  (cond ((stringp object) (%format-escape-string object))
        ((symbolp object) (symbol-name object))
        ((fixnump object) (%format-fixnum object))
        ((characterp object)
         (case object
           (#\newline "#\\\\Newline")
           (#\tab "#\\\\Tab")
           (#\space "#\\\\Space")
           (#\return "#\\\\Return")
           (t (make-string #\# #\\ #\\ object))))
        ((null object) "NIL")
        ((consp object) (%format-cons object #'%format-object-s))
        (t "??")))


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
                   (string-stream-append ss (%format-object-a arg))))
                ((or (eql #\S spec) (eql #\s spec))
                 (let ((arg (pop args)))
                   (incf i 1)
                   (string-stream-append ss (%format-object-s arg))))
                ((or (eql #\B spec) (eql #\b spec))
                 (let ((arg (pop args)))
                   (unless (fixnump arg)
                     (signal 'format-error "Not of type FIXNUM" arg))
                   (incf i 1)
                   (string-stream-append ss (%format-fixnum arg 2))))
                ((or (eql #\O spec) (eql #\o spec))
                 (let ((arg (pop args)))
                   (unless (fixnump arg)
                     (signal 'format-error "Not of type FIXNUM" arg))
                   (incf i 1)
                   (string-stream-append ss (%format-fixnum arg 8))))
                ((or (eql #\D spec) (eql #\d spec))
                 (let ((arg (pop args)))
                   (unless (fixnump arg)
                     (signal 'format-error "Not of type FIXNUM" arg))
                   (incf i 1)
                   (string-stream-append ss (%format-fixnum arg 10))))
                ((or (eql #\X spec) (eql #\x spec))
                 (let ((arg (pop args)))
                   (unless (fixnump arg)
                     (signal 'format-error "Not of type FIXNUM" arg))
                   (incf i 1)
                   (string-stream-append ss (%format-fixnum arg 16))))
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
  "Formats ARGS based on the FORMAT string and prints to STREAM.

Format specifiers begin with the ~ character and include:
    ~A and ~a : aesthetic representation of the argument
    ~S and ~s : syntactic representation
    ~B and ~b : binary
    ~O and ~o : octal
    ~D and ~d : decimal
    ~X and ~x : hexadecimal
    ~~        : literal character #\~
    ~%        : literal character #\Newline

If STREAM is NIL, the formatted string is returned instead of being
printed.

If STREAM is T, *STANDARD-OUTPUT* is used as the output stream and
NIL is returned."
  (let ((formatted (%format-impl format args)))
    (when (eq t stream)
      (setf stream *STANDARD-OUTPUT*))
    (if stream
        (progn (print formatted stream)
               nil)
        formatted)))



