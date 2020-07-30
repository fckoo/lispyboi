(in-package :lispyboi)
(provide "hash-table")
(require "defstruct")

;;; This is a fake implementation of a "hashtable" because it's just a wrapper
;;; around an alist, however, it does provide the expected interface that a
;;; real hash-table implementation would. We'll change this to a proper
;;; implementation later.

(defstruct hash-table
  (test #'eql)
  (table))

(defun hash-table-keys (hash-table)
  (map1 #'car (hash-table-table hash-table)))

(defun hash-table-values (hash-table)
  (map1 #'cdr (hash-table-table hash-table)))

(defun gethash (key hash-table &optional default)
  (let ((found (assoc key (hash-table-table hash-table) (hash-table-test hash-table))))
    (if found
        (cdr found)
        default)))

(defun sethash (key hash-table value)
  (let ((found (assoc key (hash-table-table hash-table) (hash-table-test hash-table))))
    (if found
        (setf (cdr found) value)
        (push (cons key value) (hash-table-table hash-table)))
    value))

(defsetf gethash sethash)

(export '(hash-table
          hash-table-keys
          hash-table-values
          gethash
          sethash))
