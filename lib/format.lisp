(in-package :lispyboi)
(provide "format")
(require "math")
(require "string-stream")

(defun %format-fixnum (number stream &optional (radix 10))
  (unless (fixnump number)
    (signal 'format-error "Not of type FIXNUM" number))
  (if (= number 0)
      (output-stream-write-char stream #\0)
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
          (output-stream-write-char stream #\-))
        (dolist (digit digits)
          (output-stream-write-char stream digit)))))

(defun %format-cons (cons stream format-object-func)
  (output-stream-write-char stream #\()
  (while (consp (cdr cons))
         (funcall format-object-func (car cons) stream)
         (output-stream-write-char stream #\Space)
         (setf cons (cdr cons)))
  (cond ((null (cdr cons))
         (funcall format-object-func (car cons) stream))
        (t
         (funcall format-object-func (car cons) stream)
         (output-stream-write-string stream " . ")
         (funcall format-object-func (cdr cons) stream)))
  (output-stream-write-char stream #\)))

(defun %format-object-a (object stream)
  (typecase object
    (string (output-stream-write-string stream object))
    (character (output-stream-write-char stream object))
    (cons (%format-cons object stream #'%format-object-a))
    (t (print-object object stream))))

(defun %format-object-s (object stream)
  (print-object object stream))

(defun %format-impl (format args)
  (with-output-to-string (ss)
    (let ((i 0)
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
                        (string-stream-write-char ss #\~))

                       ((eql #\% spec)
                        (incf i 1)
                        (string-stream-write-char ss #\Newline))))
                   (string-stream-write-char ss (aref format i)))
               (incf i))))))

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
        (progn (output-stream-write-string stream formatted)
               nil)
        formatted)))

(export '(format))
