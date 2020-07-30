(in-package :lispyboi)
(provide "time-it")

(defmacro time-it (time-var expr &body body)
  (let ((start-sym (gensym "TIME-START")))
    `(let ((,start-sym (get-clock-ticks)))
       ,expr
       (let ((,time-var (%/ (- (get-clock-ticks) ,start-sym)
                            1000)))
         ,@body))))

(export '(time-it))
