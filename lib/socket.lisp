
(require "ffi")

(ffi-defstruct addrinfo
               (ai-flags int)
               (ai-family int)
               (ai-socktype int)
               (ai-protocol int)
               (ai-addrlen int)
               (ai-addr void*)
               (ai-canonname char*)
               (ai-next addrinfo*))

;;(print-line (list 'ai-flags (ffi-offset-of addrinfo ai-flags)))
;;(print-line (list 'ai-family (ffi-offset-of addrinfo ai-family)))
;;(print-line (list 'ai-socktype (ffi-offset-of addrinfo ai-socktype)))
;;(print-line (list 'ai-protocol (ffi-offset-of addrinfo ai-protocol)))
;;(print-line (list 'ai-addrlen (ffi-offset-of addrinfo ai-addrlen)))
;;(print-line (list 'ai-addr (ffi-offset-of addrinfo ai-addr)))
;;(print-line (list 'ai-canonname (ffi-offset-of addrinfo ai-canonname)))
;;(print-line (list 'ai-next (ffi-offset-of addrinfo ai-next)))
;;
;;(print-line (ffi-sizeof addrinfo))

(let* ((libc (ffi-open "libc.so.6"))

       (c-fn-getaddrinfo (ffi-get-symbol libc "getaddrinfo"))
       (c-fn-freeaddrinfo (ffi-get-symbol libc "freeaddrinfo"))
       (c-fn-socket (ffi-get-symbol libc "socket"))
       (c-fn-connect (ffi-get-symbol libc "connect"))
       (c-fn-close (ffi-get-symbol libc "close"))
       (c-fn-send (ffi-get-symbol libc "send"))
       (c-fn-recv (ffi-get-symbol libc "recv"))
       (c-fn-strlen (ffi-get-symbol libc "strlen"))

       (+af-unspec+ 0)
       (+sock-stream+ 1)
       (+ai-passive+ 1))

  (defun socket-open (host port)
    (let ((host (ffi-marshal host))
          (port (ffi-marshal port))
          (result (ffi-nullptr))
          (hints (make-addrinfo)))

      (addrinfo-set-ai-family hints +af-unspec+)
      (addrinfo-set-ai-socktype hints +sock-stream+)
      (addrinfo-set-ai-flags hints +ai-passive+)

      (unwind-protect
           (let ((rc (ffi-coerce-fixnum (ffi-call c-fn-getaddrinfo host port hints (ffi-ref result)))))
             (when (< rc 0)
               (signal 'socket-error "getaddrinfo failed with code: " rc))
             (unwind-protect
                  (let* ((socket (ffi-call c-fn-socket
                                           (addrinfo-get-ai-family result)
                                           (addrinfo-get-ai-socktype result)
                                           (addrinfo-get-ai-protocol result)))
                         (socket-fixnum (ffi-coerce-fixnum socket)))
                    (when (< socket-fixnum 0)
                      (signal 'socket-error "socket acquisition failed"))

                    (let ((rc (ffi-coerce-fixnum
                               (ffi-call c-fn-connect
                                         socket
                                         (addrinfo-get-ai-addr result)
                                         (addrinfo-get-ai-addrlen result)))))
                      (when (< rc 0)
                        (signal 'socket-error "connect failed with code: " rc)))
                    socket)
               (print-line "doing freeaddrinfo")
               (ffi-call c-fn-freeaddrinfo result)))
        (print-line "doing ffi-free")
        (ffi-free hints))))

  (defun socket-close (socket)
    (let ((rc (ffi-coerce-fixnum (ffi-call c-fn-close socket))))
      (when (< rc 0)
        (signal 'socket-error "failed to close socket with code: " rc))
      t))

  (defun socket-send (socket message)
    (let* ((written 0)
           (bytes (ffi-marshal message))
           (size (ffi-coerce-fixnum (ffi-call c-fn-strlen bytes))))
      (while (< written size)
        (let ((rc (ffi-coerce-fixnum
                   (ffi-call c-fn-send socket (ffi-ref bytes written) (- size written) 0))))
          (when (< rc 0)
            (signal 'socket-error "send failed with code: " rc))
          (incf written rc)))
      written))

  (defun socket-recv (socket &optional (buffer-size-hint 1024))
    (let* ((buffer (ffi-alloc buffer-size-hint))
           (bytes-read (ffi-coerce-fixnum
                        (ffi-call c-fn-recv socket buffer buffer-size-hint 0))))
      (when (< bytes-read 0)
        (ffi-free buffer)
        (signal 'socket-error "recv failed with code: " bytes-read))
      (list buffer bytes-read)))

  (defun socket-recv-string (socket &optional (buffer-size-hint 1024))
    (let* ((recv (socket-recv socket buffer-size-hint))
           (buffer (first recv))
           (bytes-read (second recv)))
      (prog1 (ffi-coerce-string buffer bytes-read)
        (ffi-free buffer)))))

(provide "socket")
