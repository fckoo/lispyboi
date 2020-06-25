(provide "repl")

(defun repl ()
  (let ((stdin (open "/dev/stdin" 'read)))
    (until (file-eof-p stdin)
           (format t "LISPY-BOII> ")
           (handler-case (format t " ==>~s\n" (%eval (read stdin)))
             (t (&rest args)
               (format t "Unhandled signal:\n    ~s\n" args))))))


