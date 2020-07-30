(in-package :lispyboi)
(provide "defpackage")

(defmacro defpackage (package &rest options)
  (let ((use nil)
        (export nil)
        (import-from nil))
    (dolist (opt options)
      (case (car opt)
        (:use (setf use (append (cdr opt) use)))
        (:export (setf export (append (cdr opt) export)))
        (:import-from (setf import-from (append (list (cdr opt)) import-from)))))
    `(progn
       (let ((pkg (make-package ',package)))
         (export (list ,@(map (lambda (s) (typecase s
                                            (string `(intern ,s pkg))
                                            (symbol `(intern ,(symbol-name s) pkg))))
                              export))
                 pkg)
         ,@(map (lambda (p) `(use-package ',p pkg)) use)
         pkg))))


(export '(defpackage))
