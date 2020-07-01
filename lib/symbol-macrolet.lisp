(provide "symbol-macrolet")

(defmacro symbol-macrolet (macro-bindings &body body)
  (labels ((macrolet-transform (expr)
             (typecase expr
               (symbol (let ((found (assoc expr macro-bindings)))
                         (if found
                             (second found)
                             expr)))
               (cons
                (case (car expr)
                  (quote expr)
                  (lambda `(lambda ,(second expr) ,@(macrolet-transform (cddr expr))))
                  (t (map1 #'macrolet-transform expr))))
               (t expr))))
    `(progn ,@(macrolet-transform body))))
