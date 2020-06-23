(provide "defstruct")


(let ((struct-type-registry))
  (defun %defstruct (struct-name slot-descriptions)
    (let* ((struct-name-str (symbol-name struct-name))
           (ctor (intern (concatenate "MAKE-" struct-name-str)))
           (slot-names (map1 #'first slot-descriptions))
           (slot-initializers (map1 (lambda (desc) (list (first desc) (second desc)))
                                    slot-descriptions))
           (slot-indices (let ((i -1)) (map1 (lambda (e) (incf i)) slot-descriptions)))
           (getter-names (map1 (lambda (slot-name)
                                 (intern (concatenate struct-name-str "-" (symbol-name slot-name))))
                               slot-names))
           (setter-names (map1 (lambda (slot-name)
                                 (intern (concatenate struct-name-str "-SET-" (symbol-name slot-name))))
                               slot-names))
           (type (list struct-name :slot-names slot-names))
           (type-predicate (intern (concatenate struct-name-str
                                                (if (find-last-of struct-name-str #\-)
                                                    "-P"
                                                    "P")))))
      (push type struct-type-registry)
      `(progn
         (defun ,ctor (&optional ,@slot-initializers)
           (let ((instance (%create-instance ',type ,(length slot-names))))
             ,@(map (lambda (slot-name index) `(%set-slot instance ,index ,slot-name))
                    slot-names
                    slot-indices)
             instance))
         (defun ,type-predicate (object)
           (eq ',struct-name (type-of object)))
         ,@(map (lambda (getter-name index)
                  `(defun ,getter-name (instance)
                     (if (eq ',struct-name (type-of instance))
                         (%get-slot instance ,index)
                         (signal 'type-error ',struct-name instance))))
                getter-names
                slot-indices)
         ,@(map (lambda (setter-name index)
                  `(defun ,setter-name (instance value)
                     (if (eq ',struct-name (type-of instance))
                         (%set-slot instance ,index value)
                         (signal 'type-error ',struct-name instance))))
                setter-names
                slot-indices)
         ,@(map (lambda (getter setter) (list 'defsetf getter setter))
                getter-names
                setter-names)
         ',struct-name)))

  (defun type-definition (type-name)
    (assoc type-name struct-type-registry)))

(defmacro defstruct (name &rest slot-descriptions)
  (%defstruct name slot-descriptions))

(defun index-of (thing list &optional (test #'eq))
  (labels ((index-of-aux (n list)
             (cond ((null list) nil)
                   ((funcall test thing (car list)) n)
                   (t (index-of-aux (+ 1 n) (cdr list))))))
    (index-of-aux 0 list)))

(defun slot-index (object slot-name)
  (let ((type (type-definition (type-of object))))
    (if type
        (let* ((slots (second (member :slot-names (rest type))))
               (slot-index (index-of slot-name slots)))
          slot-index))))

(defun slot-value (object slot-name)
  (let ((index (slot-index object slot-name)))
    (if index
        (%get-slot object index)
        (signal 'slot-missing slot-name (type-of object)))))

(defun set-slot-value (object slot-name value)
  (let ((index (slot-index object slot-name)))
    (if index
        (%set-slot object index value)
        (signal 'slot-missing slot-name (type-of object)))))

(defsetf slot-value set-slot-value)
