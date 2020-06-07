(defmacro assert-true (expr)
  (let ((tmp-var-name (gensym)))
    `(let ((,tmp-var-name ,expr))
       (unless ,tmp-var-name
         (print "Assertion failed for ")
         (print-line ',expr)))))

(defmacro assert-false (expr)
  (let ((tmp-var-name (gensym)))
    `(let ((,tmp-var-name ,expr))
       (when ,tmp-var-name
         (print "Assertion failed for ")
         (print-line ',expr)))))

(defun %assert (test expected actual)
  (let ((actual-var-name (gensym))
        (expected-var-name (gensym)))
    `(let ((,actual-var-name ,actual)
           (,expected-var-name ,expected))
       (unless (,test ,actual-var-name ,expected-var-name)
         (print ',test)
         (print " assertion failed for ")
         (print-line ',actual)
         (print "  Expected to get ")
         (print ,expected-var-name)
         (print " but got ")
         (print ,actual-var-name)
         (print-line " instead.")))))

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

(provide "asserts")
