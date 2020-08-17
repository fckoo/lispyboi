(in-package :lispyboi)
(provide "list")


(defun index-of (thing list &optional (test #'eq))
  (labels ((index-of-aux (n list)
             (cond ((null list) nil)
                   ((funcall test thing (car list)) n)
                   (t (index-of-aux (+ 1 n) (cdr list))))))
    (index-of-aux 0 list)))

(defun nthcdr (n list)
  (while (and list (> n 0))
    (decf n)
    (setf list (cdr list)))
  list)

(defun last (list)
  (while (cdr list)
    (setf list (cdr list)))
  (car list))

(defun only (predicate seq &optional (offset 0))
  (typecase seq
    (list (if (null seq)
              nil
              (labels ((aux (list)
                         (cond ((null list) t)
                               ((funcall predicate (car list))
                                (aux (cdr list)))
                               (t nil))))
                (aux (nthcdr offset seq)))))

    (array (if (>= offset (length seq))
               nil
               (labels ((aux (n)
                          (cond ((= n (length seq)) t)
                                ((funcall predicate (aref seq n))
                                 (aux (+ 1 n)))
                                (t nil))))
                 (aux offset))))))


(defun insert-after (list value)
  (setf (cdr list) (cons value (cdr list)))
  list)

(defun insert-before (list value)
  (let ((new-rest (cons (car list) (cdr list))))
    (setf (car list) value)
    (setf (cdr list) new-rest))
  list)


(export '(index-of
          nthcdr
          only
          last
          insert-after
          insert-before))
