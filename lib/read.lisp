(in-package :lispyboi)
(provide "read")
(require "format")

(let ((readtable))
  (defun set-reader-macro (character function)
    (push (cons character function) readtable)
    character)

  (defun set-reader-macros (characters function)
    (dolist (char characters)
      (set-reader-macro char function))
    characters)

  (defun get-reader-macro (character)
    (assoc character readtable #'eql))

  (defun get-readtable ()
    readtable)

  (defun set-readtable (table)
    (setf readtable table)))

(let ((sharpsign-macros))
  (defun set-sharpsign-macro (character function)
    (push (cons character function) sharpsign-macros)
    character)

  (defun set-sharpsign-macros (characters function)
    (dolist (c characters)
      (set-sharpsign-macro c function))
    characters)

  (set-reader-macro #\#
                    (lambda (stream char)
                      (let* ((c (input-stream-read-char stream t))
                             (macro (assoc c sharpsign-macros #'eql)))
                        (if macro
                            (funcall (cdr macro) stream c)
                            (signal 'sharpsign-macro-error "There is no sharpsign (#) macro for" c))))))

(defun binary-digit-p (c)
  (<= (char-code #\0) (char-code c) (char-code #\1)))

(defun octal-digit-p (c)
  (<= (char-code #\0) (char-code c) (char-code #\7)))

(defun decimal-digit-p (c)
  (<= (char-code #\0) (char-code c) (char-code #\9)))

(defun hexadecimal-digit-p (c)
  (or (<= (char-code #\0) (char-code c) (char-code #\9))
      (<= (char-code #\a) (char-code c) (char-code #\f))
      (<= (char-code #\A) (char-code c) (char-code #\F))))

(defun symbol-char-p (c)
  (not (or (member c '(#\( #\) #\' #\" #\` #\,))
           (spacep c))))

(defun spacep (c)
  (member c '(#\Space #\Newline #\Return #\Tab)))

(defun consume-spaces (stream)
  (until (or (input-stream-eof-p stream)
             (not (spacep (input-stream-peek-char stream t))))
         (input-stream-read-char stream t)))


(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value)
  (cond ((eq nil peek-type)
         (input-stream-peek-char stream eof-error-p eof-value))
        ((characterp peek-type)
         (until (or (input-stream-eof-p stream)
                    (eql peek-type (input-stream-peek-char stream eof-error-p eof-value)))
                (input-stream-read-char stream eof-error-p eof-value))
         (input-stream-peek-char stream eof-error-p eof-value))
        (t
         (consume-spaces stream)
         (input-stream-peek-char stream eof-error-p eof-value))))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (input-stream-read-char stream eof-error-p eof-value))

(defun digit-num (digit)
  (- (char-code digit) (char-code #\0)))

(defun parse-integer (string &optional (radix 10))
  (let ((i (if (or (eql #\- (aref string 0))
                   (eql #\+ (aref string 0)))
               1
               0))
        (negativep (eql #\- (aref string 0)))
        (num 0))
    (while (< i (length string))
           (setf num (* radix num))
           (if (> (char-code (aref string i)) (char-code #\9))
               (incf num (+ 10 (- (char-code (char-upcase (aref string i))) (char-code #\A))))
               (incf num (- (char-code (aref string i)) (char-code #\0))))
           (incf i))
    (if negativep
        (- num)
        num)))

(set-reader-macro #\'
                  (lambda (stream char)
                    (list 'quote (read stream))))

(set-reader-macro #\`
                  (lambda (stream char)
                    (list 'quasiquote (read stream))))

(set-reader-macro #\,
                  (lambda (stream char)
                    (let ((sym 'unquote))
                      (when (eql #\@ (peek-char nil stream t))
                        (read-char stream t)
                        (setf sym 'unquote-splicing))
                      (list sym (read stream)))))


(set-reader-macro #\(
                  (lambda (stream char)
                    (when (eql #\) (peek-char t stream t))
                      (read-char stream t) ;; consume closing )
                      (signal 'return nil))
                    (let* ((head (list (read stream)))
                           (cur head))
                      (until (or (input-stream-eof-p stream)
                                 (eql #\) (peek-char t stream t)))
                             (when (eql #\) (peek-char t stream t))
                               (read-char stream t) ;; consume closing )
                               (signal 'return head))
                             (when (eql #\. (peek-char t stream t))
                               (read-char stream t) ;; consume .
                               (setf (cdr cur) (read stream))
                               (unless (eql #\) (peek-char t stream t))
                                 (error "Unbalanced parentheses"))
                               (read-char stream t) ;; consume closing )
                               (signal 'return head))
                             (setf (cdr cur) (list (read stream)))
                             (setf cur (cdr cur)))
                      (read-char stream t) ;; consume closing ), doesn't matter if eof
                      (signal 'return head))))

(set-reader-macro #\)
                  (lambda (stream char)
                    (error "Unbalanced parentheses")))


(set-reader-macro #\;
                  (lambda (stream char)
                    (while (eql #\; char)
                           (until (or (input-stream-eof-p stream)
                                      (eql #\Newline (peek-char nil stream t)))
                                  (read-char stream t))
                           (setf char (peek-char t stream t)))
                    (read stream t)))

(set-reader-macro #\"
                  (lambda (stream char)
                    (let ((buf (make-string-stream)))
                      (until (or (input-stream-eof-p stream)
                                 (eql #\" (peek-char nil stream t)))
                             (let ((c (read-char stream t)))
                               (if (not (eql #\\ c))
                                   (string-stream-write-char buf c)
                                   (string-stream-write-char buf
                                                             (let ((c (read-char stream t)))
                                                               (case c
                                                                 (#\n #\Newline)
                                                                 (#\t #\Tab)
                                                                 (#\r #\Return)
                                                                 (t c)))))))
                      (read-char stream t) ;; consume closing "
                      (string-stream-str buf))))


(set-sharpsign-macro #\\
                     (lambda (stream char)
                       (let ((buf (make-string-stream)))
                         (until (or (input-stream-eof-p stream)
                                    (not (symbol-char-p (peek-char nil stream t))))
                                (string-stream-write-char buf (read-char stream t)))
                         (cond ((string-stream-empty-p buf)
                                (read-char stream t))
                               ((= 1 (string-stream-length buf))
                                (string-stream-peek-char buf))
                               (t
                                (let ((str (string-upcase! (string-stream-str buf))))
                                  (cond ((string= "NEWLINE" str) #\Newline)
                                        ((string= "TAB" str) #\Tab)
                                        ((string= "SPACE" str) #\Space)
                                        ((string= "RETURN" str) #\Return)
                                        (t (signal 'reader-error "Unrecognized named character" str)))))))))

(set-sharpsign-macros '(#\x #\X)
                      (lambda (stream char)
                        (let ((buf (make-string-stream)))
                          (until (or (input-stream-eof-p stream)
                                     (not (hexadecimal-digit-p (peek-char nil stream t))))
                                 (string-stream-write-char buf (read-char stream t)))
                          (when (string-stream-empty-p buf)
                            (signal 'reader-error "Not a hexadecimal digit" (peek-char nil stream t)))
                          (parse-integer (string-stream-str buf) 16))))

(set-sharpsign-macros '(#\b #\B)
                      (lambda (stream char)
                        (let ((buf (make-string-stream)))
                          (until (or (input-stream-eof-p stream)
                                     (not (binary-digit-p (peek-char nil stream t))))
                                 (string-stream-write-char buf (read-char stream t)))
                          (when (string-stream-empty-p buf)
                            (signal 'reader-error "Not a octal digit" (peek-char nil stream t)))
                          (parse-integer (string-stream-str buf) 2))))

(set-sharpsign-macros '(#\o #\O)
                      (lambda (stream char)
                        (let ((buf (make-string-stream)))
                          (until (or (input-stream-eof-p stream)
                                     (not (octal-digit-p (peek-char nil stream t))))
                                 (string-stream-write-char buf (read-char stream t)))
                          (when (string-stream-empty-p buf)
                            (signal 'reader-error "Not a binary digit" (peek-char nil stream t)))
                          (parse-integer (string-stream-str buf) 8))))

(set-sharpsign-macro #\. (lambda (stream char) (eval (read stream))))

(set-sharpsign-macro #\' (lambda (stream char) (list 'function (read stream))))

(defun %has-package-p (string)
  (labels ((aux (i)
             (cond ((= i (length string))
                    nil)
                   ((eql #\: (aref string i))
                    (let ((pkg-name (substring string 0 i)))
                      (when (> (+ 1 i) (length string))
                        (signal 'symbol-error "Package Specifier without symbol" string))
                      (if (eql #\: (aref string (+ 1 i)))
                          (list pkg-name (substring string (+ 2 i)) t)
                          (list pkg-name (substring string (+ 1 i)) nil))))
                   (t
                    (aux (+ i 1))))))
    (aux 0)))

(defun %read-intern (string)
  (cond ((string= string "NIL")
         nil)
        ((eql #\: (aref string 0))
         (intern (substring string 1) "KEYWORD"))
        (t
         (destructuring-bind (pkg-name symbol-name interned-p)
             (%has-package-p string)
           (if pkg-name
               (intern symbol-name pkg-name)
               (intern string *package*))))))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (handler-case
      (progn
        (until (input-stream-eof-p stream)
               (consume-spaces stream)
               (let* ((c (read-char stream t))
                      (macro (get-reader-macro c)))
                 (cond (macro
                        (signal 'return (funcall (cdr macro) stream c)))
                       ((or (eql #\- c) (eql #\+ c) (decimal-digit-p c))
                        (let ((buf (make-string-stream))
                              (numberp t))
                          (string-stream-write-char buf c)
                          (until (or (input-stream-eof-p stream)
                                     (not (decimal-digit-p (peek-char nil stream t))))
                                 (string-stream-write-char buf (read-char stream t)))
                          (unless (input-stream-eof-p stream)
                            (when (symbol-char-p (peek-char nil stream t))
                              (setf numberp nil)
                              (until (or (input-stream-eof-p stream)
                                         (not (symbol-char-p (peek-char nil stream t))))
                                     (string-stream-write-char buf (read-char stream t))))
                            (when (and numberp
                                       (or (eql #\- c) (eql #\+ c))
                                       (= 1 (string-stream-length buf)))
                              (setf numberp nil)))
                          (signal 'return (if numberp
                                              (parse-integer (string-stream-str buf))
                                              (%read-intern
                                               (string-upcase! (string-stream-str buf)))))))
                       (t
                        (let ((buf (make-string-stream)))
                          (string-stream-write-char buf c)
                          (until (or (input-stream-eof-p stream)
                                     (get-reader-macro (peek-char nil stream t))
                                     (spacep (peek-char nil stream t)))
                                 (string-stream-write-char buf (read-char stream t)))
                          (signal 'return (%read-intern
                                           (string-upcase! (string-stream-str buf)))))))))
        (signal 'end-of-file))
    (return (val) val)
    (end-of-file ()
      (if eof-error-p
          (signal 'end-of-file)
          eof-value))))

(export '(set-reader-macro
          set-reader-macros
          get-reader-macro
          get-readtable
          set-readtable
          set-sharpsign-macro
          set-sharpsign-macros

          binary-digit-p
          octal-digit-p
          decimal-digit-p
          hexadecimal-digit-p
          symbol-char-p
          spacep

          peek-char
          read-char
          parse-integer

          read))
