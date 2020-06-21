(provide "defstruct")


(let ((struct-type-registry))
  (defun %defstruct (struct-name slot-descriptions)
    (let* ((ctor (intern (concatenate "MAKE-" (symbol-name struct-name))))
           (slot-names (map1 #'first slot-descriptions))
           (slot-indices (let ((i -1)) (map1 (lambda (e) (incf i)) slot-descriptions)))
           (type (list struct-name :slot-names slot-names)))
      (push type struct-type-registry)
      `(progn
         (defun ,ctor ,slot-names
           (let ((instance (%create-instance ',type ,(length slot-names))))
             ,@(map (lambda (slot-name index) `(%set-slot instance ,index ,slot-name))
                    slot-names
                    slot-indices)
             instance))
         ,@(map (lambda (slot-name index)
                  `(defun ,(intern (concatenate (symbol-name struct-name) "-" (symbol-name slot-name)))
                       (instance)
                     (%get-slot instance ,index)))
                slot-names
                slot-indices)
         ,@(map (lambda (slot-name index)
                  `(defun ,(intern (concatenate (symbol-name struct-name) "-SET-" (symbol-name slot-name)))
                       (instance value)
                     (%set-slot instance ,index value)))
                slot-names
                slot-indices)
         ,@(map (lambda (slot-name)
                  `(defsetf
                       ,(intern (concatenate (symbol-name struct-name) "-" (symbol-name slot-name)))
                       ,(intern (concatenate (symbol-name struct-name) "-SET-" (symbol-name slot-name)))))
                slot-names)))))

(defmacro defstruct (name &rest slot-descriptions)
  (%defstruct name slot-descriptions))
