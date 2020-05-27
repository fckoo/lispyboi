(setq list (lambda (&rest lst) lst))
(defmacro defun (name argslist &rest body)
  (list 'setq name (cons 'lambda (cons argslist body))))
(defun null (obj) (eq nil obj))
(defun not (obj) (if obj nil t))
(defun caar (obj) (car (car obj)))
(defun cadar (obj) (car (cdr (car obj))))
(defun get-syms (args)
  (if (null args)
      nil
      (cons (caar args) (get-syms (cdr args)))))
(defun get-vals (args)
  (if (null args)
      nil
      (cons (cadar args) (get-vals (cdr args)))))
(defmacro let (args &rest body)
  (cons (cons 'lambda (cons (get-syms args) body))
        (get-vals args)))

(defun and-helper (args)
  (if (null (cdr args))
      (car args)
      (list 'if (car args) (and-helper (cdr args)))))

(defmacro and (&rest exprs)
  (and-helper exprs))

