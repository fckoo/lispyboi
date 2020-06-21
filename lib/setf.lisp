(provide "setf")

(defun set-cadr (obj val)
  (setf (car (cdr obj)) val))

(defun set-caddr (obj val)
  (setf (car (cddr obj)) val))

(defun set-cadddr (obj val)
  (setf (car (cdddr obj)) val))

(defun set-caddddr (obj val)
  (setf (car (cddddr obj)) val))

(defsetf cadr set-cadr)
(defsetf caddr set-caddr)
(defsetf cadddr set-cadddr)
(defsetf caddddr set-caddddr)

(defsetf first %set-car)
(defsetf second set-cadr)
(defsetf third set-caddr)
(defsetf fourth set-cadddr)
(defsetf fifth set-caddddr)
