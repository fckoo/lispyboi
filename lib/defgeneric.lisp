(provide "defgeneric")
(require "typep")

;;; Currently we're only allowing a type specilization for the first argument to simplify
;;; method resolution.

(defmacro defgeneric (fun-name lambda-list)
  (let ((define-method (intern (concatenate "%DEFINE-METHOD-" (symbol-name fun-name)))))
    `(let ((methods nil))
       (defun ,fun-name (&rest args)
         (let ((func (assoc (first args) methods #'typep)))
           (if func
               (apply (cdr func) args)
               (signal 'no-method-exists "No method exists" ',fun-name (type-of (first args))))))
       (defun ,define-method (type function)
         (push (cons type function) methods)
         nil))))


(defmacro defmethod (fun-name args &body body)
  (let ((define-method (intern (concatenate "%DEFINE-METHOD-" (symbol-name fun-name)))))
    (let* ((first-type (if (consp (first args))
                           (second (first args))
                           t))
           (first-name (if (consp (first args))
                           (first (first args))
                           (first args)))
           (lambda-list (cons first-name (rest args))))
      `(progn
         ,@(when (null (%function-definition define-method))
             (list (list 'defgeneric fun-name lambda-list)))
         (,define-method
             ',first-type
             (lambda ,lambda-list ,@body))))))
