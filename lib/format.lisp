(provide "format")
(require "math")
(require "string-stream")


(defun %format-fixnum (number stream &optional (radix 10))
  (unless (fixnump number)
    (signal 'format-error "Not of type FIXNUM" number))
  (if (= number 0)
      (stream-putchar stream #\0)
      (let ((digits)
            (negp (< number 0)))
        (setf number (abs number))
        (while (/= number 0)
               (let ((rem (rem number radix)))
                 (if (> rem 9)
                     (push (code-char (+ (char-code #\A) (- rem 10))) digits)
                     (push (code-char (+ (char-code #\0) rem)) digits)))
               (setf number (floor number radix)))
        (when negp
          (stream-putchar stream #\-))
        (dolist (digit digits)
          (stream-putchar stream digit)))))

(defun %format-cons (cons stream format-object-func)
  (stream-putchar stream #\()
  (while (consp (cdr cons))
         (funcall format-object-func (car cons) stream)
         (stream-putchar stream #\Space)
         (setf cons (cdr cons)))
  (cond ((null (cdr cons))
         (funcall format-object-func (car cons) stream))
        (t
         (funcall format-object-func (car cons) stream)
         (stream-puts stream " . ")
         (funcall format-object-func (cdr cons) stream)))
  (stream-putchar stream #\)))

(defun %format-object-a (object stream)
  (typecase object
    (string (stream-puts stream object))
    (character (stream-putchar stream object))
    (cons (%format-cons object stream #'%format-object-a))
    (t (print-object object stream))))

(defun %format-object-s (object stream)
  (print-object object stream))

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
                      (incf i 1)
                      (%format-object-a (pop args) ss))

                     ((or (eql #\S spec) (eql #\s spec))
                      (incf i 1)
                      (%format-object-s (pop args) ss))

                     ((or (eql #\B spec) (eql #\b spec))
                      (incf i 1)
                      (%format-fixnum (pop args) ss 2))

                     ((or (eql #\O spec) (eql #\o spec))
                      (incf i 1)
                      (%format-fixnum (pop args) ss 8))

                     ((or (eql #\D spec) (eql #\d spec))
                      (incf i 1)
                      (%format-fixnum (pop args) ss 10))

                     ((or (eql #\X spec) (eql #\x spec))
                      (incf i 1)
                      (%format-fixnum (pop args) ss 16))

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
    ~~        : literal character #\\~
    ~%        : literal character #\\Newline

If STREAM is NIL, the formatted string is returned instead of being
printed.

If STREAM is T, *STANDARD-OUTPUT* is used as the output stream and
NIL is returned."
  (let ((formatted (%format-impl format args)))
    (when (eq t stream)
      (setf stream *STANDARD-OUTPUT*))
    (if stream
        (progn (stream-puts stream formatted)
               nil)
        formatted)))
