(in-package :lispyboi)
(provide "ssl")
(require "ffi")

(defpackage lispyboi.ssl
  (:use lispyboi
        lispyboi.socket)
  (:export ssl-context
           make-default-ssl-context
           ssl-wrap-socket
           ssl-read
           ssl-read1
           ssl-read-string))

(in-package :lispyboi.ssl)

;; just need to link libcrypto for libssl to function properly
(ffi-with-symbols "libcrypto.so.1.1" ())

(ffi-with-symbols
 "libssl.so.1.1"
 ((c-tls-v1.2-client-method "TLSv1_2_client_method")
  (c-ssl-ctx-new "SSL_CTX_new")
  (c-ssl-ctx-free "SSL_CTX_free")
  (c-ssl-new "SSL_new")
  (c-ssl-free "SSL_free")
  (c-ssl-get-peer-certificate "SSL_get_peer_certificate")
  (c-ssl-set-fd "SSL_set_fd")
  (c-ssl-connect "SSL_connect")
  (c-ssl-read "SSL_read")
  (c-ssl-peek "SSL_peek")
  (c-ssl-write "SSL_write")
  (c-ssl-pending "SSL_pending"))
 
 (defun %ssl-create-default-context ()
   (let* ((method (c-tls-v1.2-client-method))
          (ctx (c-ssl-ctx-new method)))
     (when (ffi-nullptr-p ctx)
       (signal 'ssl-init "Failed to initialize SSL Context"))
     (list (c-ssl-new ctx) ctx)))
 
 (defun %ssl-wrap-socket (ssl socket)
   (c-ssl-set-fd ssl socket)
   (c-ssl-connect ssl)
   ssl)

 (defun %ssl-write (ssl message)
   (let* ((written 0)
          (bytes (ffi-marshal message))
          (size (ffi-strlen bytes)))
     (while (< written size)
            (let ((rc (ffi-coerce-int (c-ssl-write ssl bytes size))))
              (when (< rc 0)
                (signal 'ssl-error "SSL_write failed with" rc))
              (incf written rc)))
     written))

 (defun %ssl-pending (ssl)
   (ffi-coerce-int (c-ssl-pending ssl)))

 (defun %ssl-read (ssl &optional (buffer-size-hint 1024))
   (when buffer-size-hint
     (let* ((buffer (ffi-alloc buffer-size-hint))
            (bytes-read (ffi-coerce-int
                         (c-ssl-read ssl buffer buffer-size-hint))))
       (when (< bytes-read 0)
         (ffi-free buffer)
         (signal 'ssl-error "SSL_read failed with" bytes-read))
       (list buffer bytes-read))))

 (defun %ssl-read1 (ssl)
   (let* ((read (%ssl-read ssl 1))
          (buffer (first read)))
     (prog1 (aref (ffi-coerce-string buffer 1) 0)
       (ffi-free buffer))))

 (defun %ssl-read-pending (ssl)
   (let ((pending-bytes (%ssl-pending ssl)))
     (when (> pending-bytes 0)
       (%ssl-read ssl pending-bytes))))

 (defun %ssl-read-pending-string (ssl)
   (let ((read (%ssl-read ssl (%ssl-pending ssl))))
     (when read
       (let ((buffer (first read))
             (size (second read)))
         (prog1 (ffi-coerce-string buffer size)
           (ffi-free buffer))))))

 (defun %ssl-peek (ssl &optional (buffer-size-hint 1024))
   (let* ((buffer (ffi-alloc buffer-size-hint))
          (bytes-peeked (ffi-coerce-int
                         (c-ssl-peek ssl buffer buffer-size-hint))))
     (when (< bytes-peeked 0)
       (ffi-free buffer)
       (signal 'ssl-error "SSL_peek failed with" bytes-peeked))
     (list buffer bytes-peeked)))

 (defun %ssl-peek1 (ssl)
   (let* ((read (%ssl-peek ssl 1))
          (buffer (first read)))
     (prog1 (aref (ffi-coerce-string buffer 1) 0)
       (ffi-free buffer)))))

(defstruct ssl-context (ssl) (ctx))

(defun make-default-ssl-context ()
  (destructuring-bind (ssl ctx) (%ssl-create-default-context)
    (make-ssl-context ssl ctx)))

(defun ssl-wrap-socket (ssl-context socket)
  (%ssl-wrap-socket (ssl-context-ssl ssl-context) (socket-fd socket)))

(defun ssl-read (ssl-context &optional (buffer-size-hint 1024))
  (with-ffi-array (array byte (%ssl-read (ssl-context-ssl ssl-context) buffer-size-hint))
    array))

(defun ssl-read1 (ssl-context)
  (%ssl-read1 (ssl-context-ssl ssl-context)))

(defun ssl-read-string (ssl-context)
  (with-ffi-array (array character (%ssl-read-pending (ssl-context-ssl ssl-context)))
    array))

(defmethod output-stream-write-char ((stream ssl-context) character)
  (%ssl-write (ssl-context-ssl stream) (make-string character)))

(defmethod output-stream-write-string ((stream ssl-context) string)
  (%ssl-write (ssl-context-ssl stream) string))



