(require "socket")
(require "ssl")

(setq *irc-server* "IRC HOSTNAME")
(setq *irc-port* 6697)

(let ((ssl (make-default-ssl-context))
      (socket (socket-open *irc-server* *irc-port*))
      (code-handlers nil))
  (ssl-wrap-socket ssl socket)

  (defun read-line ()
    (let ((ss (make-string-stream)))
      (let ((c (ssl-read1 ssl)))
        (until (eql #\newline c)
               (unless (eql #\return c)
                 (string-stream-write-char ss c))
               (setf c (ssl-read1 ssl)))
        (string-stream-str ss))))
  
  (defun privmsg (target message)
    (format nil "PRIVMSG ~a :~a~%" target message))

  (defun join (channel &optional (key ""))
    
    (format nil "JOIN ~a ~a~%" channel key))

  (defun part (channel &optional (message ""))
    (format nil "PART ~a ~a~%" channel message))

  (defun set-user (username &optional (message "a"))
    (format nil "USER ~a nil nil :~a~%" username message))

  (defun set-nick (nickname)
    (format nil "NICK ~a~%" nickname))

  (defun pong (key)
    (format nil "PONG ~a~%" key))

  (defun quit-msg (&optional (msg ""))
    (format nil "QUIT ~a~%" msg))

  (defun send-raw (message)
    (format t "~a" message)
    (format ssl "~a" message))
  
  (send-raw (set-user "lispyboi" "pewpew"))
  (send-raw (set-nick "lispyboi"))

  (defun def-code-handler (code handler)
    (push (cons code handler) code-handlers))

  (defun dispatch-code (code args)
    (let ((handler (assoc code code-handlers #'string=)))
      (when handler
        (apply (cdr handler) code args))))

  (let ((channel "#xxx"))
    (def-code-handler "PRIVMSG"
        (lambda (code sender channel message)
          (cond ((string= "!test" message)
                 (send-raw (privmsg
                            channel
                            (format nil
                                    "beep boop o/ ~a"
                                    (first (string-split sender #\! 1)))))))))

    (def-code-handler "396"
        (lambda (code &rest args)
          (send-raw (join channel))))

    (def-code-handler "366"
        (lambda (code &rest args)
          (send-raw (privmsg channel "hihihi"))))

    (dotimes (i 5000)
      (let* ((line (read-line))
             (parts (string-split line #\: 2)))
        (format t "~a~%" line)
        (when parts
          (cond ((string= "PING " (first parts))
                 (send-raw (format nil "PONG :~a~%" (second parts))))
                ((string= "" (first parts))
                 (destructuring-bind (sender code . rest) (string-split (second parts))
                   (dispatch-code code `(,sender ,@rest ,(third parts)))))))))))
