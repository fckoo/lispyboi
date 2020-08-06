(in-package :lispyboi)
(provide "error")

(defun error (fmt &rest args)
  (if (stringp fmt)
      (signal 'error (apply #'format nil fmt args))
      (apply #'kernel::%signal fmt args)))

(defmacro error (fmt &rest args)
  (if (stringp fmt)
      `(signal 'error (format nil ,fmt ,@args))
      `(signal 'error ,fmt ,@args)))

(defun warn (fmt &rest args)
  (format *standard-error* "WARNING: ~A~%" (apply #'format nil fmt args)))

(export '(error warn))
