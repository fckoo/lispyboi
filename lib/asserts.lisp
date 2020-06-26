(provide "asserts")

(defmacro assert-true (expr)
  (let ((tmp-var-name (gensym)))
    `(let ((,tmp-var-name ,expr))
       (if ,tmp-var-name
           'pass
           (progn
             (format t "Assertion failed for ~s~%" ',expr)
             'fail)))))

(defmacro assert-false (expr)
  (let ((tmp-var-name (gensym)))
    `(let ((,tmp-var-name ,expr))
       (if ,tmp-var-name
           (progn
             (format t "Assertion failed for ~s~%" ',expr)
             'fail)
           'pass))))

(defun %assert (test expected actual)
  (let ((actual-var-name (gensym))
        (expected-var-name (gensym)))
    `(let ((,actual-var-name ,actual)
           (,expected-var-name ,expected))
       (if (,test ,actual-var-name ,expected-var-name)
           'pass
           (progn
             (format t "~a assertion failed for ~a~%" ,(format nil "~s" test) ,(format nil "~s" actual))
             (format t "  Expected to get ~s but got ~s instead.~%" ,expected-var-name ,actual-var-name)
             'fail)))))

(defmacro assert-eq (expected actual)
  (%assert 'eq expected actual))

(defmacro assert-eql (expected actual)
  (%assert 'eql expected actual))

(defmacro assert-equal (expected actual)
  (%assert 'equal expected actual))

(defmacro assert-= (expected actual)
  (%assert '= expected actual))

(defmacro assert-/= (expected actual)
  (%assert '/= expected actual))

(defmacro assert-string= (expected actual)
  (%assert 'string= expected actual))

(defmacro assert-string/= (expected actual)
  (%assert 'string/= expected actual))


