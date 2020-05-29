(defmacro cons (x y) (%cons '%cons (%cons x (%cons y nil))))
(setq cons (lambda (x y) (cons x y)))

(setq list (lambda (&rest lst) lst))

(defmacro defun (name argslist &rest body)
  (list 'setq name (cons 'lambda (cons argslist body))))

(defmacro car (obj) (list '%car obj))
(defun car (obj) (car obj))

(defmacro cdr (obj) (list '%cdr obj))
(defun cdr (obj) (cdr obj))

(defmacro eq (x y) (list '%eq x y))
(defun eq (x y) (eq x y))

(defmacro type-of (obj) (list '%type-of obj))
(defun type-of (obj) (type-of obj))

(defmacro read () (list '%read nil))
(defun read () (read))

(defmacro macro-expand (expr) (list '%macro-expand expr))
(defun macro-expand (expr) (macro-expand expr))

(defmacro eval (expr) (list '%eval (macro-expand expr)))
(defun eval (expr) (eval expr))

(defmacro apply (func &rest args)
  (cons '%apply (cons func args)))
(defun apply (func &rest args)
  ;; this is incorrect.
  (eval (cons '%apply (cons (list 'quote func) args))))

(defmacro - (&rest vals) (cons '%- vals))
(defun - (&rest vals) (apply %- vals))

(defmacro + (&rest vals) (cons '%+ vals))
(defun + (&rest vals) (apply %+ vals))

(defmacro * (&rest vals) (cons '%* vals))
(defun * (&rest vals) (apply %* vals))

(defmacro < (&rest vals) (cons '%< vals))
(defun < (&rest vals) (apply %< vals))

(defmacro putchar (character) (list '%putchar character))
(defun putchar (character) (putchar character))

(defmacro print (obj) (list '%print obj))
(defun print (obj) (print obj))

(defun null (obj) (eq nil obj))
(defun not (obj) (if obj nil t))
(defun cadr (lst) (car (cdr lst)))
(defun cadar (lst) (car (cdr (car lst))))
(defun caddr (lst) (car (cdr (cdr lst))))
(defun cadddr (lst) (car (cdr (cdr (cdr lst)))))
(defun caddddr (lst) (car (cdr (cdr (cdr (cdr lst))))))
(defun caar (lst) (car (car lst)))
(defun caaar (lst) (car (car (car lst))))
(defun caadr (lst) (car (car (cdr lst))))
(defun caaadr (lst) (car (car (car (cdr lst)))))
(defun caaaadr (lst) (car (car (car (car (cdr lst))))))
(defun cddr (lst) (cdr (cdr lst)))
(defun cdddr (lst) (cdr (cdr (cdr lst))))
(defun cddddr (lst) (cdr (cdr (cdr (cdr lst)))))
(defun cdddddr (lst) (cdr (cdr (cdr (cdr (cdr lst))))))
(defmacro first (lst) (list 'car lst))
(defmacro rest (lst) (list 'cdr lst))
(defmacro second (lst) (list 'cadr lst))
(defmacro third (lst) (list 'caddr lst))
(defmacro fourth (lst) (list 'cadddr lst))
(defmacro fifth (lst) (list 'caddddr lst))

(defun append (x y)
  (if x
      (cons (car x) (append (cdr x) y))
      y))

(defmacro let (args &rest body)
  (defun get-syms (args)
    (if (null args)
        nil
        (cons (caar args) (get-syms (cdr args)))))
  (defun get-vals (args)
    (if (null args)
        nil
        (cons (cadar args) (get-vals (cdr args)))))
  (cons (cons 'lambda (cons (get-syms args) body))
        (get-vals args)))

(defmacro and (&rest exprs)
  (defun and-helper (args)
    (if (null (cdr args))
        (car args)
        (list 'if (car args) (and-helper (cdr args)))))
  (and-helper exprs))

(defun consp (obj) (eq 'cons (type-of obj)))
(defun symbolp (obj) (eq 'symbol (type-of obj)))

(defmacro quasiquote (exp)
  (defun qq-list (l)
    (if (consp l)
        (let ((obj (first l)))
          (if (and (consp obj) (eq (first obj) 'unquote-splicing))
              (if (rest l)
                  (list 'append (second obj) (qq-list (rest l)))
                  (second obj))
              (list 'cons (qq-object obj) (qq-list (rest l)))))
        (list 'quote l)))

  (defun qq-element (l)
    (if (eq (first l) 'unquote)
        (second l)
        (qq-list l)))

  (defun qq-object (object)
    (if (consp object)
        (qq-element object)
        (list 'quote object)))
  (qq-object exp))


(defmacro progn (&body body)
  `((lambda () ,@body)))

(defmacro when (test &body body)
  `(if ,test (progn ,@body) nil))

(defmacro unless (test &body)
  `(if ,test nil (progn ,@body)))

(defmacro cond (&body body)
  (if (null body)
      nil
      `(if ,(caar body)
           (progn ,@(rest (first body)))
           (cond ,@(rest body)))))

(defun assoc (item alist)
  (when alist
    (if (eq item (caar alist))
        (car alist)
        (assoc item (cdr alist)))))


;; @HACK: this gets the defun to bind to this scope...
(setq %defsetf nil)
(setq get-setf-expansion nil)
(let ((*setf-functions* nil))
  (defun %defsetf (access-fn update-fn)
    (setq *setf-functions* (cons (cons access-fn update-fn) *setf-functions*))
    access-fn)

  (defun get-setf-expansion (form)
    (cond ((symbolp form)
           form)
          ((consp form)
           (let ((set-functions (assoc (car form) *setf-functions*)))
             (when set-functions
               (append (list (cdr set-functions))
                       (rest form)))))
          (t
           ;; @TODO: Need to implement error capabilities in host
           nil))))

(defmacro defsetf (access-fn update-fn)
  (%defsetf access-fn update-fn)
  `(quote ,access-fn))

(defsetf car %set-car)
(defsetf cdr %set-cdr)

(defmacro setf (place value)
  (let ((expansion (get-setf-expansion place)))
    (cond ((symbolp expansion)
           `(setq ,expansion ,value))
          ((consp expansion)
           (append expansion (list value)))
          (t
           ;; @TODO: Need to implement error capabilities in host
           nil))))
