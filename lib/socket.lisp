(in-package :lispyboi)
(provide "socket")
(require "ffi")
(require "format")

(defpackage lispyboi.socket
  (:use lispyboi)
  (:export socket
           make-socket
           socketp
           socket-fd
           socket-error
           socket-open
           socket-open-server
           socket-close
           socket-alive-p
           socket-send
           socket-recv
           socket-recv-string
           socket-accept))

(in-package :lispyboi.socket)



(ffi-with-symbols
 "libc.so.6"
 ((c-getaddrinfo "getaddrinfo")
  (c-freeaddrinfo "freeaddrinfo")
  (c-socket "socket")
  (c-connect "connect")
  (c-close "close")
  (c-send "send")
  (c-recv "recv")
  (c-listen "listen")
  (c-bind "bind")
  (c-accept "accept")
  (c-htons "htons")
  (c-htonl "htonl")
  (c-fcntl "fcntl")
  (c-setsockopt "setsockopt"))
 
 (ffi-defstruct addrinfo
                (ai-flags int32)
                (ai-family int32)
                (ai-socktype int32)
                (ai-protocol int32)
                (ai-addrlen int32)
                (ai-addr void*)
                (ai-canonname char*)
                (ai-next addrinfo*))

 (ffi-defstruct in-addr
                (s-addr uint32))

 (ffi-defstruct sockaddr-in
                (sin-family int16)
                (sin-port uint16)
                (sin-addr in-addr)
                ;; this is supposed to be char[8] but unsupported atm
                (sin-zero[0] char)
                (sin-zero[1] char)
                (sin-zero[2] char)
                (sin-zero[3] char)
                (sin-zero[4] char)
                (sin-zero[5] char)
                (sin-zero[6] char)
                (sin-zero[7] char))

 (let ((+af-unspec+ 0)
       (+af-inet+ 2)
       (+ai-passive+ 1)
       (+sock-stream+ 1)
       (+inaddr-any+ 0)

       (+f-getfl+ 3)
       (+f-setfl+ 4)
       (+o-nonblock+ 2048)

       (+ewouldblock+ 11)
       (+econnaborted+ 103))

   (defun %socket-get-flags (socket)
     (ffi-coerce-int (c-fcntl socket +f-getfl+ 0)))

   (defun %socket-set-flags (socket new-flags)
     (prog1 (%socket-get-flags socket)
       (ffi-coerce-int (c-fcntl socket +f-setfl+ new-flags))))

   (defun %socket-add-flags (socket new-flags)
     (let ((flags (%socket-get-flags socket)))
       (when (< flags 0)
         (%socket-close socket)
         (signal 'socket-error "unable to add socket flags." (errno-str)))
       (%socket-set-flags socket (bit-ior flags new-flags))))

   (defun %socket-open (host port)
     (let ((host (ffi-marshal host))
           (port (ffi-marshal port))
           (result (ffi-nullptr))
           (hints (make-addrinfo)))

       (setf (addrinfo.ai-family hints) +af-unspec+)
       (setf (addrinfo.ai-socktype hints) +sock-stream+)
       (setf (addrinfo.ai-flags hints) +ai-passive+)

       (unwind-protect
            (let ((rc (ffi-coerce-int
                       (c-getaddrinfo host port hints (ffi-ref result)))))
              (when (< rc 0)
                (signal 'socket-error "getaddrinfo failed with code: " rc))
              (unwind-protect
                   (let* ((socket (c-socket
                                   (addrinfo.ai-family result)
                                   (addrinfo.ai-socktype result)
                                   (addrinfo.ai-protocol result)))
                          (rc (ffi-coerce-int socket)))
                     (when (< rc 0)
                       (signal 'socket-error "socket acquisition failed" (errno-str)))
                     (let ((rc (ffi-coerce-int
                                (c-connect
                                 socket
                                 (addrinfo.ai-addr result)
                                 (addrinfo.ai-addrlen result)))))
                       (when (< rc 0)
                         (signal 'socket-error "connect failed with code: " rc)))
                     socket)
                (c-freeaddrinfo result)))
         (ffi-free hints))))

   (defun %socket-open-nonblock (host port)
     (let ((socket (%socket-open host port)))
       (%socket-add-flags socket +o-nonblock+) 
       socket))

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
         (ffi-free buffer))))

   (defun %socket-open-server (port &rest flags)
     (let ((server-addr (make-sockaddr-in))
           (port (c-htons port))
           (family +af-inet+)
           (addr +inaddr-any+)
           (listen-fd (c-socket +af-inet+ +sock-stream+ 0)))

       (when flags
         (let ((n 0))
           (dolist (flag flags)
             (case flag
               (:nonblock (setf n (bit-ior n +o-nonblock+)))
               (:ndelay (setf n (bit-ior n +o-nonblock+)))))
           (unless (= 0 n)
             (%socket-add-flags listen-fd n))))

       (unwind-protect
            (progn
              (setf (sockaddr-in.sin-family server-addr) family)
              (setf (in-addr.s-addr (sockaddr-in.sin-addr server-addr))
                    addr)
              (setf (sockaddr-in.sin-port server-addr) port)

              (when (/= 0 (ffi-coerce-int (c-bind listen-fd server-addr (ffi-sizeof sockaddr-in))))
                (signal 'socket-error "bind failed" (errno-str)))

              (when (/= 0 (ffi-coerce-int (c-listen listen-fd 10)))
                (signal 'socket-error "listen failed" (errno-str)))
              
              listen-fd)
         (ffi-free server-addr))))

   (defun %socket-accept (socket)
     (let ((comm-fd (c-accept socket 0 0)))
       (if (< (ffi-coerce-int comm-fd) 0)
           (case (errno)
             (+ewouldblock+ nil)
             (+econnaborted+ nil)
             (otherwise (signal 'socket-error "accept failed" (errno-str))))
           comm-fd)))

   (defun %socket-alive-p (socket)
     t)

   ))

(defstruct socket (fd))

(defun check-port (port)
  (unless (fixnump port)
    (signal 'socket-error "Port must be a FIXNUM in the range (0-65535)" port))
  (unless (< 0 port #x10000)
    (signal 'socket-error "Port out of range (0-65535)" port)))

(defun socket-open (host port)
  (check-port port)
  (make-socket :fd (%socket-open host (format nil "~d" port))))

(defun socket-close (socket)
  (when socket
    (let ((int-socket (socket-fd socket)))
      (when int-socket
        (prog1 (%socket-close int-socket)
          (setf (socket-fd socket) nil))))))

(defun socket-send (socket message)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-SEND: Socket internal FD is NIL"))
    (%socket-send int-socket message)))

(defun socket-recv (socket &optional (buffer-size-hint 1024))
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-RECV: Socket internal FD is NIL"))
    (%socket-recv int-socket buffer-size-hint)))

(defun socket-recv-string (socket &optional (buffer-size-hint 1024))
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-RECV-STRING: Socket internal FD is NIL"))
    (%socket-recv-string int-socket buffer-size-hint)))

(defun socket-open-server (port &rest flags)
  (check-port port)
  (make-socket :fd (apply #'%socket-open-server port flags)))

(defun socket-accept (socket)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "SOCKET-ACCEPT: Socket internal FD is NIL"))
    (let ((accepted (%socket-accept int-socket)))
      (if accepted
          (list (make-socket :fd accepted) nil nil)
          (list nil nil nil)))))

(defun socket-alive-p (socket)
  (let ((int-socket (socket-fd socket)))
    (and int-socket (%socket-alive-p int-socket))))

(defmethod output-stream-write-char ((socket socket) character)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "OUTPUT-STREAM-WRITE-CHAR: Socket internal FD is NIL"))
    (%socket-send int-socket (make-string character))))

(defmethod output-stream-write-string ((socket socket) string)
  (let ((int-socket (socket-fd socket)))
    (unless int-socket
      (signal 'socket-error "OUTPUT-STREAM-WRITE-STRING: Socket internal FD is NIL"))
    (%socket-send int-socket string)))

