(in-package :lispyboi)
(provide "sort")

(defun %merge-lists! (head list-1 list-2 predicate)
  (cond ((null list-1) list-2)
        ((null list-2) list-1)
        (t
         (let ((tail head)
               (key-1 (car list-1))
               (key-2 (car list-2)))
           (tagbody loop
              ;; keys are "backwards" for stability
              (cond ((funcall predicate key-2 key-1)
                     (setf (cdr tail) list-2)
                     (setq tail (cdr tail))
                     (setq list-2 (cdr list-2))
                     (if list-2
                         (setq key-2 (car list-2))
                         (progn
                           (setf (cdr tail) list-1)
                           (go loop-exit))))
                    (t
                     (setf (cdr tail) list-1)
                     (setq tail (cdr tail))
                     (setq list-1 (cdr list-1))
                     (if list-1
                         (setq key-1 (car list-1))
                         (progn
                           (setf (cdr tail) list-2)
                           (go loop-exit)))))
              (go loop)
            loop-exit)
           (cdr head)))))

(defun merge-lists (list-1 list-2 predicate)
  (%merge-lists! (cons nil nil) list-1 list-2 predicate))

;; This is a textbook implementation of a top-down merge sort. There is
;; a noticable performance improvement from using the top-down algorithm
;; over the bottom-up approach.
(defun sort-list! (list predicate)
  "Destructively modifies LIST to sort it by funcalling PREDICATE where
the first argument is preceded by the second. Then returns the head of
the sorted list."
  (let ((tmp (cons nil nil)))
    (labels ((find-middle (list)
               (let ((slow list)
                     (fast list))
                 (while (cddr fast)
                   (setf slow (cdr slow))
                   (setf fast (cddr fast)))
                 slow))
             (recur (list)
               (cond ((null (cdr list))
                      list)
                     (t
                      (let* ((middle (find-middle list))
                             (next (cdr middle)))
                        (setf (cdr middle) nil)
                        (let ((left (recur list))
                              (right (recur next)))
                          (%merge-lists! tmp left right predicate)))))))
      (recur list))))


(defun sort-list (list predicate)
  (sort-list! (copy-list list) predicate))


(defun sortedp (list predicate)
  (let ((sorted t))
    (while (and sorted (cdr list))
      (setf sorted (funcall predicate (first list) (second list)))
      (setf list (cdr list)))
    sorted))

(export '(merge-lists
          sort-list!
          sort-list
          sortedp))
