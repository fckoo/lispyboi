(provide "read")

(let ((readtable))
  (defun set-reader-macro (character function)
    (push (cons character function) (car readtable))
    character)

  (defun set-reader-macros (characters function)
    (dolist (char characters)
      (set-reader-macro char function))
    characters)

  (defun get-reader-macro (character)
    (assoc character (car readtable) #'eql))

  (defun push-readtable ()
    (push (list) readtable)
    t)

  (defun pop-readtable ()
    (pop readtable)
    t)

  ;; setup initial, empty readtable
  (push-readtable))

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
                      (let* ((c (stream-getc stream t :eof))
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

(defun nthcdr (n list)
  (while (/= n 0)
         (decf n)
         (setf list (cdr list)))
  list)

(defun only (predicate seq &optional (offset 0))
  (typecase seq
    (list (if (null seq)
              nil
              (labels ((aux (list)
                         (cond ((null list) t)
                               ((funcall predicate (car list))
                                (aux (cdr list)))
                               (t nil))))
                (aux (nthcdr offset seq)))))

    (array (if (>= offset (length seq))
               nil
               (labels ((aux (n)
                          (cond ((= n (length seq)) t)
                                ((funcall predicate (aref seq n))
                                 (aux (+ 1 n)))
                                (t nil))))
                 (aux offset))))))

(defun spacep (c)
  (member c '(#\Space #\Newline #\Return #\Tab)))

(defun consume-spaces (stream)
  (until (or (stream-eof-p stream)
             (not (spacep (stream-peekc stream t :eof))))
         (stream-getc stream t :eof)))


(defun digit-num (digit)
  (- (char-code digit) (char-code #\0)))

(defun char-downcase (c)
  (if (<= (char-code #\A) (char-code c) (char-code #\Z))
      (code-char (bit-ior (char-code c) 32))
      c))

(defun char-upcase (c)
  (if (<= (char-code #\a) (char-code c) (char-code #\z))
      (code-char (bit-xor (char-code c) 32))   
      c))

(defun string-upcase! (string)
  "Modifies STRING, changing all ASCII characters to their uppercase counterparts."
  (dotimes (i (length string))
    (when (<= 0 (char-code (aref string i)) 127)
      (setf (aref string i) (char-upcase (aref string i)))))
  string)

(defun string-downcase! (string)
  "Modifies STRING, changing all ASCII characters to their lowercase counterparts."
  (dotimes (i (length string))
    (when (<= 0 (char-code (aref string i)) 127)
      (setf (aref string i) (char-downcase (aref string i)))))
  string)


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
                      (when (eql #\@ (stream-peekc stream t :eof))
                        (stream-getc stream t :eof)
                        (setf sym 'unquote-splicing))
                      (list sym (read stream)))))


(set-reader-macro #\(
                  (lambda (stream char)
                    (consume-spaces stream)
                    (when (eql #\) (stream-peekc stream t :eof))
                      (stream-getc stream t :eof) ;; consume closing )
                      (signal 'return nil))
                    (let* ((head (list (read stream)))
                           (cur head))
                      (consume-spaces stream)
                      (until (or (stream-eof-p stream)
                                 (eql #\) (stream-peekc stream t :eof)))
                             (when (eql #\) (stream-peekc stream t :eof))
                               (stream-getc stream t :eof) ;; consume closing )
                               (signal 'return head))
                             (when (eql #\. (stream-peekc stream t :eof))
                               (stream-getc stream t :eof) ;; consume .
                               (setf (cdr cur) (read stream))
                               (consume-spaces stream)
                               (unless (eql #\) (stream-peekc stream t :eof))
                                 (error "Unbalanced parentheses"))
                               (stream-getc stream t :eof) ;; consume closing )
                               (signal 'return head))
                             (setf (cdr cur) (list (read stream)))
                             (setf cur (cdr cur))
                             (consume-spaces stream))
                      (stream-getc stream t :eof) ;; consume closing ), doesn't matter if eof
                      (signal 'return head))))

(set-reader-macro #\)
                  (lambda (stream char)
                    (error "Unbalanced parentheses")))


(set-reader-macro #\;
                  (lambda (stream char)
                    (while (eql #\; char)
                           (until (or (stream-eof-p stream)
                                      (eql #\Newline (stream-peekc stream t :eof)))
                                  (stream-getc stream t :eof))
                           (consume-spaces stream)
                           (setf char (stream-peekc stream t :eof)))
                    (read stream)))

(set-reader-macro #\"
                  (lambda (stream char)
                    (let ((buf (make-string-stream)))
                      (until (or (stream-eof-p stream)
                                 (eql #\" (stream-peekc stream t :eof)))
                             (let ((c (stream-getc stream t :eof)))
                               (if (not (eql #\\ c))
                                   (string-stream-push buf c)
                                   (string-stream-push buf
                                                       (let ((c (stream-getc stream t :eof)))
                                                         (case c
                                                           (#\n #\Newline)
                                                           (#\t #\Tab)
                                                           (#\r #\Return)
                                                           (t c)))))))
                      (stream-getc stream t :eof) ;; consume closing "
                      (string-stream-str buf))))


(set-sharpsign-macro #\\
                     (lambda (stream char)
                       (let ((buf (make-string-stream)))
                         (until (or (stream-eof-p stream)
                                    (not (symbol-char-p (stream-peekc stream t :eof))))
                                (string-stream-push buf (stream-getc stream t :eof)))
                         (cond ((string-stream-empty-p buf)
                                (stream-getc stream t :eof))
                               ((= 1 (string-stream-length buf))
                                (string-stream-peekc buf))
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
                          (until (or (stream-eof-p stream)
                                     (not (hexadecimal-digit-p (stream-peekc stream t :eof))))
                                 (string-stream-push buf (stream-getc stream t :eof)))
                          (when (string-stream-empty-p buf)
                            (signal 'reader-error "Not a hexadecimal digit" (stream-peekc stream t :eof)))
                          (parse-integer (string-stream-str buf) 16))))

(set-sharpsign-macros '(#\b #\B)
                      (lambda (stream char)
                        (let ((buf (make-string-stream)))
                          (until (or (stream-eof-p stream)
                                     (not (binary-digit-p (stream-peekc stream t :eof))))
                                 (string-stream-push buf (stream-getc stream t :eof)))
                          (when (string-stream-empty-p buf)
                            (signal 'reader-error "Not a octal digit" (stream-peekc stream t :eof)))
                          (parse-integer (string-stream-str buf) 2))))

(set-sharpsign-macros '(#\o #\O)
                      (lambda (stream char)
                        (let ((buf (make-string-stream)))
                          (until (or (stream-eof-p stream)
                                     (not (octal-digit-p (stream-peekc stream t :eof))))
                                 (string-stream-push buf (stream-getc stream t :eof)))
                          (when (string-stream-empty-p buf)
                            (signal 'reader-error "Not a binary digit" (stream-peekc stream t :eof)))
                          (parse-integer (string-stream-str buf) 8))))

(set-sharpsign-macro #\. (lambda (stream char) (eval (read stream))))

(set-sharpsign-macro #\' (lambda (stream char) (list 'function (read stream))))


(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (handler-case
      (progn
        (until (stream-eof-p stream)
               (consume-spaces stream)
               (let* ((c (stream-getc stream t :eof))
                      (macro (get-reader-macro c)))
                 (cond (macro
                        (signal 'return (funcall (cdr macro) stream c)))
                       ((or (eql #\- c) (eql #\+ c) (decimal-digit-p c))
                        (let ((buf (make-string-stream))
                              (numberp t))
                          (string-stream-push buf c)
                          (until (or (stream-eof-p stream)
                                     (not (decimal-digit-p (stream-peekc stream t :eof))))
                                 (string-stream-push buf (stream-getc stream t :eof)))
                          (when (symbol-char-p (stream-peekc stream t :eof))
                            (setf numberp nil)
                            (until (or (stream-eof-p stream)
                                       (not (symbol-char-p (stream-peekc stream t :eof))))
                                   (string-stream-push buf (stream-getc stream t :eof))))
                          (when (and numberp
                                     (or (eql #\- c) (eq #\+ c))
                                     (= 1 (string-stream-length buf)))
                            (setf numberp nil))
                          (signal 'return (if numberp
                                              (parse-integer (string-stream-str buf))
                                              (let ((string (string-upcase! (string-stream-str buf))))
                                                (if (string= string "NIL")
                                                    nil
                                                    (intern string)))))))
                       (t
                        (let ((buf (make-string-stream)))
                          (string-stream-push buf c)
                          (until (or (stream-eof-p stream)
                                     (get-reader-macro (stream-peekc stream t :eof))
                                     (spacep (stream-peekc stream t :eof)))
                                 (string-stream-push buf (stream-getc stream t :eof)))
                          (signal 'return (let ((string (string-upcase! (string-stream-str buf))))
                                            (if (string= string "NIL")
                                                nil
                                                (intern string)))))))))
        (signal 'end-of-file))
    (return (val) val)
    (end-of-file ()
      (if eof-error-p
          (signal 'end-of-file)
          eof-value))))

