(require "socket")
(require "sleep")
(provide "httpd")

(defpackage httpd 
  (:use lispyboi
        lispyboi.socket
        lispyboi.sleep)
  (:export http-error
           get head post put delete connect
           options trace patch))

(in-package :httpd)


(defstruct http-header
  (method) (path) (protocol)
  (user-agent)
  (other-headers)
  (request))

(defun try-parse-integer (string &optional (radix 10))
  (ignore-errors (parse-integer string radix)))

(defun parse-protocol (protocol-string)
  (destructuring-bind (http ver)
      (string-split protocol-string #\/ 1)
    (when (string/= "HTTP" (string-upcase! http))
      (signal 'http-error 400 "Malformed protocol string: ~a" protocol-string))
    (destructuring-bind (major minor)
        (map1 #'try-parse-integer (string-split ver #\. 1))
      (unless (fixnump major)
        (signal 'http-error 400 "Unrecognized HTTP major version: ~a" protocol-string))
      (unless (fixnump minor)
        (signal 'http-error 400 "Unrecognized HTTP minor version: ~a" protocol-string))
      (list major minor))))

(let ((method-map
        (map1 (lambda (s) (cons (symbol-name s) s))
              '(get head post put delete connect
                options trace patch))))
  (defun parse-method (method-string)
    (or (cdr (assoc method-string method-map #'string=))
        (signal 'http-error 400 "Unrecognized method: ~a" method-string))))

(defun parse-http-next (header-string)
  (destructuring-bind (current rest)
      (string-split header-string #\Return 1)
    (list (string-trim current)
          rest)))

(defun unescape-path (path)
  (let ((ss (make-string-stream))
        (i 0))
    (while (< i (length path))
           (cond ((eql #\+ (aref path i))
                  (incf i)
                  (string-stream-write-char ss #\Space))
                 ((eql #\% (aref path i))
                  (string-stream-write-char ss (code-char (parse-integer (substring path (+ i 1) (+ i 3)) 16)))
                  (incf i 3))
                 (t
                  (string-stream-write-char ss (aref path i))
                  (incf i))))
    (string-stream-str ss)))

(defun parse-http-header (header-string)
  (let ((method) (path) (protocol)
        (user-agent)
        (other-headers)
        (header-lines (parse-http-next header-string)))
    (destructuring-bind (meth pat prot . rest)
        (string-split (first header-lines) #\Space 3)
      (when rest
        (signal 'http-error 400 "Too many literal spaces: ~a" (first header-lines)))
      (setf method (parse-method meth))
      (setf path (unescape-path pat))
      (setf protocol (parse-protocol prot)))
    (let ((lines (parse-http-next (second header-lines))))
      ;; go until no more lines or we hit an empty lines
      (until (or (null (first lines))
                 (= 0 (length (first lines))))
             (destructuring-bind (key value)
                 (string-split (first lines) #\: 1)
               (when (and key value)
                 (push (cons key (string-trim value)) other-headers)))
             (setf lines (parse-http-next (second lines))))
      (setf user-agent (cdr (assoc "User-Agent" other-headers #'string=)))
      (make-http-header method path protocol
                        user-agent
                        other-headers
                        (if (second lines)
                            (string-trim-left (second lines))
                            (make-string))))))



(let ((status-code-message-map))
  (defun http-status-code-add (status-code message)
    (push (cons status-code message) status-code-message-map))

  (http-status-code-add 100 "Continue")

  (http-status-code-add 200 "OK")
  (http-status-code-add 201 "Created")
  (http-status-code-add 202 "Accepted")

  (http-status-code-add 300 "Multiple Choice")
  (http-status-code-add 301 "Moved Permanently")
  (http-status-code-add 302 "Found")
  (http-status-code-add 307 "Temporary Redirect")
  (http-status-code-add 308 "Permanent Redirect")

  (http-status-code-add 400 "Bad Request")
  (http-status-code-add 401 "Unauthorized")
  (http-status-code-add 403 "Forbidden")
  (http-status-code-add 404 "Not Found")
  (http-status-code-add 405 "Method Not Allowed")
  (http-status-code-add 406 "Not Acceptable")
  (http-status-code-add 411 "Length Required")
  (http-status-code-add 412 "Precondition Failed")
  (http-status-code-add 413 "Payload Too Large")
  (http-status-code-add 414 "URI Too Long")
  (http-status-code-add 415 "Unsupported Media Type")
  (http-status-code-add 429 "Too Many Requests")
  (http-status-code-add 431 "Request Header Fields Too Large")

  (http-status-code-add 500 "Internal Server Error")
  (http-status-code-add 501 "Not Implemented")
  (http-status-code-add 502 "Bad Gateway")
  (http-status-code-add 503 "Service Unavailable")
  (http-status-code-add 505 "HTTP Version Not Supported")
  
  (defun http-status-code-message (status-code)
    (cdr (assoc status-code status-code-message-map #'=))))

(defun http-error-response (error-code)
  (array-join "\r\n"
              (format nil "HTTP/1.0 ~a ~a" error-code (http-status-code-message error-code))
              (format nil "Connection: close")
              (format nil "Content-Length: 0")
              (format nil "")))

(defun http-ok (content)
  (array-join "\r\n"
              (format nil "HTTP/1.0 200 OK")
              (format nil "Connection: close")
              (format nil "Content-Length: %d" (length content))
              (format nil "Content-Type: text/html; UTF-8")
              (format nil "")
              content))

(defun seconds-from-now (n)
  (+ (get-clock-ticks) (* (clocks-per-second) n)))

(defstruct http-client
  (socket)
  (address)
  (port)
  (time-to-live (seconds-from-now 60)))

(setq *dummy-page*
      "<html>
    <body>
        <h1>Hello world</h1>
        <p>~s<p>
    </body>
</html>")

(defun http-handle-one (client content)
  (let* ((socket (http-client-socket client))
         (recv (socket-recv-string socket))
         (timestamp (timestamp)))
    (handler-case
        (let ((header (parse-http-header recv)))
          (format t "~a ~a ~s~%"
                  timestamp (http-header-method header) (http-header-path header))
          (socket-send socket (http-ok content)))
      (http-error (code &rest args)
        (format t "~a HTTP ERROR ~d: ~s~%" timestamp code (apply #'format nil args))
        (socket-send socket (http-error-response code)))
      (t (tag &rest args)
        (format t "~a INTERNAL ERROR ~s: ~s~%" timestamp tag args)
        (socket-send socket (http-error-response 400))))
    (socket-close socket)
    t))

(defun run-simple-http-server (port &optional (max-connections 20))
  (let* ((server (socket-open-server port :nonblock))
         (clients (list nil)))
    (while t
           (when (< (length clients) max-connections)
             (destructuring-bind (client-socket client-addr client-port)
                 (socket-accept server)
               (when client-socket
                 (push (make-http-client client-socket client-addr client-port)
                       clients))))
           (let ((clients clients))
             (while (car clients)
                    (let ((socket (http-client-socket (car clients))))
                      (if (socket-alive-p socket)
                          (if (http-handle-one (car clients)
                                               (format nil *dummy-page*
                                                       (format nil "yer a b00t and The current time is ~s"
                                                               (datetime-now))))
                              (pop! clients))
                          (progn
                            (socket-close socket)
                            (pop! clients))))
                    (setf clients (cdr clients))))
           (msleep 250))))

(run-simple-http-server 8080)
