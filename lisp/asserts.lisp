(defmacro assert-true (expr)
  (let ((tmp-var-name (gensym)))
    `(let ((,tmp-var-name ,expr))
       (unless ,tmp-var-name
         (print '(assertion failed for ,expr))))))

(defmacro assert-false (expr)
  (let ((tmp-var-name (gensym)))
    `(let ((,tmp-var-name ,expr))
       (when ,tmp-var-name
         (print '(assertion failed for ,expr))))))


(defmacro assert-eq (expected actual)
  (let ((actual-var-name (gensym))
        (expected-var-name (gensym)))
    `(let ((,actual-var-name ,actual)
           (,expected-var-name ,expected))
       (unless (eq ,actual-var-name ,expected-var-name)
         (print '(assertion failed for ,actual))
         (print (list 'expected 'to 'get ,expected-var-name 'but 'got ,actual-var-name 'instead))))))
