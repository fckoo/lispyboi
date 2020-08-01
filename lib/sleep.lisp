(in-package :lispyboi)
(provide "sleep")
(require "ffi")

(defpackage lispyboi.sleep
  (:use lispyboi)
  (:export usleep
           msleep
           sleep))

(in-package :lispyboi.sleep)

(ffi-with-symbols
 "libc.so.6"
 ((c-usleep "usleep"))

 (let ((+us-per-ms+ 1000)
       (+us-per-s+ 1000000))

   (defun usleep (microseconds)
     (c-usleep microseconds))

   (defun msleep (milliseconds)
     (c-usleep (* milliseconds +us-per-ms+)))

   (defun sleep (seconds)
     (c-usleep (* seconds +us-per-s+)))))


