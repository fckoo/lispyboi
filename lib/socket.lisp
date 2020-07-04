(provide "socket")
(require "ffi")
(require "format")

(ffi-with-symbols
 "libc.so.6"
 ((c-getaddrinfo "getaddrinfo")
  (c-freeaddrinfo "freeaddrinfo")
  (c-socket "socket")
  (c-connect "connect")
  (c-close "close")
  (c-send "send")
  (c-recv "recv"))

 (ffi-defstruct addrinfo
                (ai-flags int)
                (ai-family int)
                (ai-socktype int)
                (ai-protocol int)
                (ai-addrlen int)
                (ai-addr void*)
                (ai-canonname char*)
                (ai-next addrinfo*))

 (let ((+af-unspec+ 0)
       (+sock-stream+ 1)
       (+ai-passive+ 1))

   (defun %socket-open (host port)
     (let ((host (ffi-marshal host))
           (port (ffi-marshal port))
           (result (ffi-nullptr))
           (hints (make-addrinfo)))

       (addrinfo-set-ai-family hints +af-unspec+)
       (addrinfo-set-ai-socktype hints +sock-stream+)
       (addrinfo-set-ai-flags hints +ai-passive+)

       (unwind-protect
            (let ((rc (ffi-coerce-int
                       (c-getaddrinfo host port hints (ffi-ref result)))))
              (when (< rc 0)
                (signal 'socket-error "getaddrinfo failed with code: " rc))
              (unwind-protect
                   (let* ((socket (c-socket
                                   (addrinfo-get-ai-family result)
                                   (addrinfo-get-ai-socktype result)
                                   (addrinfo-get-ai-protocol result)))
                          (rc (ffi-coerce-int socket)))
                     (when (< rc 0)
                       (signal 'socket-error "socket acquisition failed"))
                     (let ((rc (ffi-coerce-int
                                (c-connect
                                 socket
                                 (addrinfo-get-ai-addr result)
                                 (addrinfo-get-ai-addrlen result)))))
                       (when (< rc 0)
                         (signal 'socket-error "connect failed with code: " rc)))
                     socket)
                (c-freeaddrinfo result)))
         (ffi-free hints))))

   (defun %socket-close (socket)
     (let ((rc (ffi-coerce-int (c-close socket))))
       (when (< rc 0)
         (signal 'socket-error "failed to close socket with code: " rc))
       t))

   (defun %socket-send (socket message)
     (let* ((written 0)
            (bytes (ffi-marshal message))
            (size (ffi-strlen bytes)))
       (while (< written size)
              (let ((rc (ffi-coerce-fixnum
                         (c-send socket (ffi-ref bytes written) (- size written) 0))))
                (when (< rc 0)
                  (signal 'socket-error "send failed with code: " rc))
                (incf written rc)))
       written))

   (defun %socket-recv (socket buffer-size)
     (let* ((buffer (ffi-alloc buffer-size))
            (bytes-read (ffi-coerce-fixnum
                         (c-recv socket buffer buffer-size 0))))
       (when (< bytes-read 0)
         (ffi-free buffer)
         (signal 'socket-error "recv failed with code: " bytes-read))
       (list buffer bytes-read)))

   (defun %socket-recv-string (socket buffer-size)
     (let* ((recv (%socket-recv socket buffer-size))
            (buffer (first recv))
            (bytes-read (second recv)))
       (prog1 (ffi-coerce-string buffer bytes-read)
         (ffi-free buffer))))))


(defstruct socket (fd))

(defun socket-open (host port)
  (when (fixnump port)
    (when (or (< port 0) (> port #xFFFF))
      (signal 'socket-error "SOCKET-OPEN: Port out of range (0-65535)" port))
    (setf port (format nil "~d" port)))
  (make-socket (%socket-open host port)))

(defun socket-close (socket)
  (let ((int-socket (socket-fd socket)))
    (when int-socket
      (prog1 (%socket-close int-socket)
        (setf (socket-fd socket) nil)))))

(defun socket-send (socket message)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-SEND: Socket internal FD is NIL"))
    (%socket-send int-socket message)))

(defun socket-recv (socket &optional (buffer-size-hint 1024))
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-RECV: Socket internal FD is NIL"))
    (%socket-send int-socket message)))

(defun socket-recv-string (socket &optional (buffer-size-hint 1024))
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-RECV-STRING: Socket internal FD is NIL"))
    (%socket-recv-string int-socket buffer-size-hint)))

(defmethod stream-putchar ((stream socket) character)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "STREAM-PUTCHAR: Socket internal FD is NIL"))
    (%socket-send int-socket (make-string character))))

(defmethod stream-puts ((stream socket) string)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "STREAM-PUTS: Socket internal FD is NIL"))
    (%socket-send int-socket string)))
