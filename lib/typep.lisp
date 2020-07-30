(in-package :lispyboi)
(provide "typep")

(defun %typep-complex-compare (want-type actual-type)
  (labels ((compare-aux (a b)
             (if (null a)
                 t
                 (let ((want (car a)) (actual (car b)))
                   (if (or (eq '* want) (eq want actual))
                       (compare-aux (cdr a) (cdr b)))))))
    (compare-aux want-type actual-type)))

(defun typep (object type)
  (let ((object-type (type-of object)))
    (if (consp type)
        (%typep-complex-compare type object-type)
        (cond ((eq object-type type) t)
              ((eq 'string type) (stringp object))
              ((eq 'array type) (arrayp object))
              ((eq 'list type) (listp object))
              ((eq t type) t)
              (t nil)))))

(export '(typep))
