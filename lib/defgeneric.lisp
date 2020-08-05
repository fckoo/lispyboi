(in-package :lispyboi)
(provide "defgeneric")
(require "typep")

;;; Currently we're only allowing a type specilization for the first argument to simplify
;;; method resolution.

(defun %get-define-method-name (name)
  (concatenate "%DEFINE-METHOD-" (symbol-name name)))

(defmacro defgeneric (fun-name lambda-list &rest documentation)
  (unless (kernel::%function-definition fun-name)
    ;;(signal 'generic-already-exists "Generic function already exists" fun-name)
    (let ((define-method (intern (%get-define-method-name fun-name))))
      (export (list define-method))
      `(let ((methods nil))
         (defun ,fun-name (&rest args)
           (let ((func (assoc (first args) methods #'typep)))
             (if func
                 (apply (cdr func) args)
                 (signal 'no-method-exists-error "No method exists" ',fun-name (type-of (first args))
                         ',fun-name methods))))
         (defun ,define-method (type function)
           (push (cons type function) methods)
           nil)))))

(defmacro defmethod (fun-name args &body body)
  (let ((define-method (intern (%get-define-method-name fun-name))))
    (let* ((first-type (if (consp (first args))
                           (second (first args))
                           t))
           (first-name (if (consp (first args))
                           (first (first args))
                           (first args)))
           (lambda-list (cons first-name (rest args))))
      `(progn
         ,@(when (null (kernel::%function-definition define-method))
             (list (list 'defgeneric fun-name lambda-list)))
         (,define-method
             ',first-type
             (lambda ,lambda-list ,@body))))))

(export '(defgeneric
          defmethod
          no-method-exists-error))
