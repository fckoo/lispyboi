(provide "symbol-macrolet")

(defmacro symbol-macrolet (macro-bindings &body body)
  `(progn ,@(%lexical-walk-replace body
                                   (lambda (e)
                                     (second (assoc e macro-bindings))))))
