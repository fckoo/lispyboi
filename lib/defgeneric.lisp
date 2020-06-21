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


(defmacro defmethod (fun-name lambda-list &body body)
  (let ((define-method (intern (concatenate "%DEFINE-METHOD-" (symbol-name fun-name)))))
    (let ((first-type (if (consp (first lambda-list))
                          (second (first lambda-list))
                          t))
          (first-name (if (consp (first lambda-list))
                          (first (first lambda-list))
                          (first lambda-list))))
      `(,define-method
           ',first-type
           (lambda ,(cons first-name (rest lambda-list))
             ,@body)))))


