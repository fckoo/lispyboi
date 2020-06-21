(provide "hash-table")

;;; This is a fake implementation of a "hashtable" because it's just a wrapper
;;; around an alist, however, it does provide the expected interface that a
;;; real hash-table implementation would. We'll change this to a proper
;;; implementation later.

(defun make-hash-table (&optional (test #'eql))
  (cons test nil))

(defun hash-table-keys (hashtable)
  (map1 #'car (cdr hashtable)))

(defun hash-table-values (hashtable)
  (map1 #'cdr (cdr hashtable)))

(defun gethash (key hashtable &optional default)
  (let ((found (assoc key (cdr hashtable) (car hashtable))))
    (if found
        (cdr found)
        default)))

(defun sethash (key hashtable value)
  (let ((found (assoc key (cdr hashtable) (car hashtable))))
    (if found
        (setf (cdr found) value)
        (push (cons key value) (cdr hashtable)))
    value))

(defsetf gethash sethash)
