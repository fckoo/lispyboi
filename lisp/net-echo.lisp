(require "socket")
(provide "net-echo")

(defpackage net-echo.server
  (:use lispyboi
        lispyboi.socket))

(in-package :net-echo.server)


(let* ((server (socket-open-server 10002))
       (client (socket-accept server)))
  (while t
         (let ((recv (socket-recv-string client)))
           (format t ">~a" recv)
           (socket-send client recv))))



(defpackage net-echo.client
  (:use lispyboi
        lispyboi.socket))

(in-package :net-echo.client)
