(kernel::%in-package :lispyboi)

(kernel::%export
 '(list
   cons
   lambda
   defun
   defmacro
   defconstant
   defvar
   &optional &rest &body
   &key &allow-other-keys
   car
   cdr
   setq
   tagbody
   go
   handler-case

   package-name
   make-package
   find-package
   in-package
   use-package
   import
   export
   find-symbol

   eval
   apply
   funcall

   - + * / < = > /= <= >=
   bit-not bit-and bit-ior bit-xor bit-shift

   putchar
   null
   not

   caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   caddddr

   first
   rest
   second
   third
   fourth
   fifth

   gensym
   symbol-package
   symbol-name
   make-symbol
   intern

   exit
   signal
   error
   errno
   errno-str

   make-list
   append
   foldl
   foldl-for-effect
   map1
   map
   filter1
   copy-list

   reverse
   reverse!

   assoc
   member

   and
   or
   symbol-macrolet
   let
   let*
   flet
   labels

   quote
   quasiquote
   unquote
   unquote-splicing

   identity

   progn
   prog1
   when
   unless
   while
   until

   type-of
   type-error
   consp
   symbolp
   numberp
   fixnump
   listp
   arrayp
   characterp
   stringp

   eq
   eql
   equal
   string=
   string/=
   char=
   char/=

   cond
   case
   ecase
   caselet
   typecase
   etypecase
   typecaselet

   get-setf-functions
   get-setf-expansion
   defsetf
   setf
   incf
   defc

   dolist
   dotimes

   push
   push!
   pop
   pop!

   make-array
   make-string
   array-type
   simple-array
   aref

   nth
   elt

   length
   min
   max

   char-code
   code-char

   substring

   print
   print-line
   macro-print
   read

   get-clock-ticks
   clocks-per-second
   operating-system

   unwind-protect
   ignore-errors
   with-open-file
   concatenate
   end-of-file
   load)
 :lispyboi)


(kernel::%define-macro lambda (lambda-list &body body)
                       (kernel::%cons 'kernel::%lambda (kernel::%cons lambda-list body)))

(kernel::%define-function 'list (lambda (&rest lst) lst))

(kernel::%define-macro cons (x y) (list 'kernel::%cons x y))

(kernel::%define-function 'cons (lambda (x y) (cons x y)))

(kernel::%define-macro defmacro (name argslist &body body)
                       (cons 'kernel::%define-macro (cons name (cons argslist body))))

(defmacro defun (name argslist &rest body)
  (list 'kernel::%define-function (list 'quote name) (cons 'lambda (cons argslist body))))

(defmacro car (obj) (list '%car obj))
(defun car (obj) (car obj))

(defmacro cdr (obj) (list '%cdr obj))
(defun cdr (obj) (cdr obj))

(defmacro eq (x y) (list '%eq x y))
(defun eq (x y) (eq x y))

(defmacro setq (x y &rest documentation) (list 'kernel::%setq x y))
(defmacro tagbody (&body body) (cons 'kernel::%tagbody body))
(defmacro go (tag) (list 'kernel::%go tag))
(defmacro handler-case (&body body) (cons 'kernel::%handler-case body))

(defmacro package-name (package-designator) (list 'kernel::%package-name package-designator))
(defun package-name (package-designator) (package-name package-designator))

(defmacro find-package (package) (list 'kernel::%find-package package))
(defun find-package (string-designator) (find-package string-designator))

(defmacro make-package (package) (list 'kernel::%make-package package))
(defun make-package (package) (make-package package))

(defmacro in-package (package) (list 'kernel::%in-package package))
(defmacro use-package (to-use &optional (in-package *package*)) (list 'kernel::%use-package to-use in-package))

(defun export (symbols &optional (package *package*)) (kernel::%export symbols package))
(defun import (symbols &optional (package *package*)) (kernel::%import symbols package))
(defun find-symbol (string &optional (package *package*)) (kernel::%find-symbol string package))

(defmacro type-of (obj) (list 'kernel::%type-of obj))
(defun type-of (obj) (type-of obj))

(defun consp (obj) (eq 'cons (type-of obj)))

(defun symbolp (obj) (eq 'symbol (type-of obj)))

(defun eval (expr)
  (kernel::%eval expr))

(defmacro apply (func &rest args)
  (cons 'kernel::%apply (cons func args)))

(defmacro - (&rest vals) (cons 'kernel::%- vals))
(defun - (&rest vals) (apply #'kernel::%- vals))

(defmacro + (&rest vals) (cons 'kernel::%+ vals))
(defun + (&rest vals) (apply #'kernel::%+ vals))

(defmacro * (&rest vals) (cons 'kernel::%* vals))
(defun * (&rest vals) (apply #'kernel::%* vals))

(defmacro / (x y) (list 'kernel::%/ x y))
(defun / (x y) (/ x y))

(defmacro < (&rest vals) (cons 'kernel::%< vals))
(defun < (&rest vals) (apply #'kernel::%< vals))

(defmacro = (&rest vals) (cons 'kernel::%= vals))
(defun = (&rest vals) (apply #'kernel::%= vals))

(defmacro > (&rest vals) (cons 'kernel::%> vals))
(defun > (&rest vals) (apply #'kernel::%> vals))

(defmacro /= (&rest vals) (list 'not (cons 'kernel::%= vals)))
(defun /= (&rest vals) (not (apply #'kernel::%= vals)))

(defmacro putchar (character &optional (stm *standard-output*))
  (list 'kernel::%file-putchar stm character))
(defun putchar (character &optional (stm *standard-output*))
  (putchar character stm))

(defun null (obj) (eq nil obj))
(defun not (obj) (if obj nil t))
(defun identity (x) x)

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun caaaar (x) (car (car (car (car x)))))
(defun caaadr (x) (car (car (car (cdr x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun caaddr (x) (car (car (cdr (cdr x)))))
(defun cadaar (x) (car (cdr (car (car x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cdaaar (x) (cdr (car (car (car x)))))
(defun cdaadr (x) (cdr (car (car (cdr x)))))
(defun cdadar (x) (cdr (car (cdr (car x)))))
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun cddaar (x) (cdr (cdr (car (car x)))))
(defun cddadr (x) (cdr (cdr (car (cdr x)))))
(defun cdddar (x) (cdr (cdr (cdr (car x)))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))

(defun first (lst) (car lst))
(defun rest (lst) (cdr lst))
(defun second (lst) (cadr lst))
(defun third (lst) (caddr lst))
(defun fourth (lst) (cadddr lst))
(defun fifth (lst) (car (cddddr lst)))

(defmacro gensym (&optional (hint "G")) (list 'kernel::%gensym hint))
(defun gensym (&optional (hint "G")) (gensym hint))

(defmacro symbol-package (symbol) (list 'kernel::%symbol-package symbol))
(defun symbol-package (symbol) (symbol-package symbol))

(defmacro symbol-name (symbol) (list 'kernel::%symbol-name symbol))
(defun symbol-name (symbol) (symbol-name symbol))

(defmacro make-symbol (symbol-name) (list 'kernel::%make-symbol symbol-name))
(defun make-symbol (symbol-name) (make-symbol symbol-name))

(defun intern (symbol-name &optional (package *package*))
  ;;(kernel::%print package symbol-name)
  (kernel::%intern symbol-name package))

(defmacro exit (&optional (n 0)) (list 'kernel::%exit n))
(defun exit (&optional (n 0)) (exit n))

(defmacro signal (tag &rest arguments)
  (cons 'kernel::%signal (cons tag arguments)))

(defun signal (tag &rest arguments)
  (apply #'kernel::%signal tag arguments))

(defun error (message &rest arguments)
  (apply #'kernel::%signal 'error message arguments))

(defmacro progn (&body body)
  (if (null (cdr body))
      (car body)
      (list (cons 'lambda (cons 'nil body)))))

(defmacro when (test &body body)
  (list 'if test (cons 'progn body) nil))

(defmacro unless (test &body body)
  (list 'if test nil (cons 'progn body)))

(defun append (x y)
  (if x
      (cons (car x) (append (cdr x) y))
      y))

(defun foldl (func init list)
  (if (null list)
      init
      (foldl func
             (funcall func (first list) init)
             (rest list))))

(defun foldl-for-effect (func list)
  (when list
    (funcall func (first list))
    (foldl-for-effect func (rest list))))

(defun map1 (function list)
  ((lambda (head tail)
     (when list
       (setq head (cons (funcall function (car list)) nil))
       (setq tail head)
       (setq list (cdr list))
       (tagbody
        loop
          (when list
            (kernel::%rplacd tail (cons (funcall function (car list)) nil))
            (setq tail (cdr tail))
            (setq list (cdr list))
            (go loop))))
     head)
   nil nil))

(defun copy-list (list)
  (map1 #'identity list))

(defun map (func &rest seqs)
  ;; NOT TAIL RECURSIVE, but a later redefinition is
  (if (null (cdr seqs))
      (map1 func (car seqs))
      (if (car seqs)
          (cons (apply func (map1 #'car seqs))
                (apply #'map func (map1 #'cdr seqs))))))

(defmacro let (args &body body)
  (cons (cons 'lambda (cons (map #'first args) body))
        (map #'second args)))

(defmacro let* (args &body body)
  (let ((names (map #'first args))
        (vals (map #'second args)))
    (let ((setqs (map (lambda (name val)
                        (list 'setq name val))
                      names
                      vals)))
      (cons (cons 'lambda (cons names (append setqs body)))
            (map (lambda (&rest p) nil) names)))))

(defun make-list (length &optional initial-element)
  (let ((list nil))
    (tagbody
     loop
       (when (> length 0)
         (setq list (cons initial-element list))
         (setq length (- length 1))
         (go loop)))
    list))

(defun reverse (list)
  (foldl #'cons nil list))

(defun reverse! (list)
  (let ((prev nil) (next nil) (curr list))
    (tagbody
     loop
       (when curr
         (setq next (cdr curr))
         (kernel::%rplacd curr prev)
         (setq prev curr)
         (setq curr next)
         (go loop)))
    prev))

(defun assoc (item alist &optional (test #'eq))
  (when alist
    (if (funcall test item (caar alist))
        (car alist)
        (assoc item (cdr alist) test))))

(defun member (item list &optional (test #'eql))
  (when list
    (if (funcall test item (car list))
        list
        (member item (cdr list) test))))

(defmacro cond (&body body)
  (if (null body)
      nil
      (if (eq t (caar body))
          (append (list 'progn) (rest (first body)))
          (list 'if (caar body)
                (append (list 'progn) (rest (first body)))
                (append (list 'cond) (rest body))))))

(defun %and-helper (args)
  (if (null (cdr args))
                 (car args)
                 (list 'if (car args) (%and-helper (cdr args)))))

(defmacro and (&rest exprs)
  (%and-helper exprs))


(defun %lexical-walk-replace (expr replace-function &optional (test #'eq) do-not-replace)
  "This gives us the ability to walk an expression tree and optionally replace atoms and
subexpressions with the ability to recognize when things probably shouldn't be replaced.

This will recognize when an expression is a QUOTE and will not modify it.

This will also recognize when a new lexical scope is formed via LAMBDA and append the
lambda-list to the DO-NOT-REPLACE list before walking the lambda's body.

The DO-NOT-REPLACE list acts as a way to immediately tell whether we should bother with
asking the REPLACE-FUNCTION for a replacement. Its only real usage is internally but
may be provided or left NIL."
  (if (member expr do-not-replace test)
      expr
      (if (consp expr)
          (cond ((eq 'quote (car expr))
                 expr)
                ((eq 'lambda (car expr))
                 (cons 'lambda
                       (cons (second expr)
                             (let ((new-dnr (append do-not-replace (second expr))))
                               (map (lambda (e)
                                      (%lexical-walk-replace e
                                                             replace-function
                                                             test
                                                             new-dnr))
                                    (cddr expr))))))
                (t
                 (let ((replaced (funcall replace-function expr)))
                   (if replaced
                       replaced
                       (map (lambda (e)
                              (%lexical-walk-replace e
                                                     replace-function
                                                     test
                                                     do-not-replace))
                            expr)))))
          (let ((replaced (funcall replace-function expr)))
            (if replaced
                replaced
                expr)))))

(defmacro symbol-macrolet (macro-bindings &body body)
  (cons 'progn (%lexical-walk-replace body
                                      (lambda (e)
                                        (second (assoc e macro-bindings))))))

(defun %flet-transform (old-new-names expr)
  (let* ((func (lambda (e)
                 (if (consp e)
                     (if (eq 'function (first e))
                         (cdr (assoc (second e) old-new-names))
                         (let ((found (assoc (first e) old-new-names)))
                           (when found
                             (cons 'funcall
                                   (cons (cdr found)
                                         (map1 (lambda (e)
                                                 (if (consp e)
                                                     (%lexical-walk-replace e func)
                                                     e))
                                               (cdr e)))))))
                     (assoc e old-new-names)))))
    (%lexical-walk-replace expr func)))

;; The basic premise for FLET and LABELS is to create new NOT INTERNED symbols for each
;; function and then walk the form's body and replace instances where the user-defined
;; name is in the call position with the appropriate uninterned symbol. LABELS does
;; exactly the same as FLET but it also transforms the body of the function definitions
;; aswell.
(defmacro flet (definitions &body body)
  ;; we do this macro expansion so the transformation happens to the lowest level code.
  ;; the purpose of that is for e.g.
  ;; (case (get-thing)
  ;;    (quote (labels-func-a))
  ;;    (lambda (labels-func-b))
  ;; ...)
  ;; If this code was inside a function in a labels, we wouldn't correctly fix the names
  ;; because our transform code would think they are truly quoted or a lambda.
  (setq body (kernel::%macro-expand body))
  (let* ((names (map #'first definitions))
         (new-names (map (lambda (e) (gensym (symbol-name e))) names))
         (old-new-names (map #'cons names new-names))
         (lambda-lists (map #'second definitions))
         (bodies (map (lambda (e) (kernel::%macro-expand (cddr e))) definitions)))
    (cons 'let (cons (map (lambda (sym ll body)
                            (list sym (cons 'lambda (cons ll body))))
                          new-names
                          lambda-lists
                          bodies)
                     (%flet-transform old-new-names body)))))



(defmacro labels (definitions &body body)
  (setq body (kernel::%macro-expand body))
  (let* ((names (map #'first definitions))
         (new-names (map (lambda (e) (gensym (symbol-name e))) names))
         (old-new-names (map #'cons names new-names))
         (lambda-lists (map #'second definitions))
         (bodies (map (lambda (e) (kernel::%macro-expand (cddr e))) definitions)))
    (cons 'let* (cons (map (lambda (sym ll body)
                             (list sym (cons 'lambda (cons ll (%flet-transform old-new-names body)))))
                           new-names
                           lambda-lists
                           bodies)
                      (%flet-transform old-new-names body)))))

(defun list-length (list)
  (let ((n 0))
    (tagbody
     loop
       (when list
         (setq n (+ n 1))
         (setq list (cdr list))
         (go loop)))
    n))

(defun map (function &rest lists)
  (if (null (cdr lists))
      (map1 function (car lists))
      (let* ((result (make-list (list-length (car lists))))
             (tail result)
             (args (make-list (list-length lists)))
             (args-tails)
             (tmp-lists))
        (tagbody loop
           (when (car lists)
             ;; Copy the head of every list into args
             (setq args-tails args)
             (setq tmp-lists lists)
             (tagbody loop
                (when args-tails
                  (kernel::%rplaca args-tails (caar tmp-lists))
                  (setq tmp-lists (cdr tmp-lists))
                  (setq args-tails (cdr args-tails))
                  (go loop)))
             
             ;; Store the result
             (kernel::%rplaca tail (apply function args))
             (setq tail (cdr tail))
             
             ;; Set every list to its tail
             (setq tmp-lists lists)
             (tagbody loop
                (when (car tmp-lists)
                  (kernel::%rplaca tmp-lists (cdar tmp-lists))
                  (setq tmp-lists (cdr tmp-lists))
                  (go loop)))
             (go loop)))
        result)))

(defun filter1 (func seq)
  (labels ((filter1-aux (accum list)
             (if list
                 (if (funcall func (car list))
                     (filter1-aux (cons (car list) accum) (cdr list))
                     (filter1-aux accum (cdr list)))
                 (reverse! accum))))
    (filter1-aux nil seq)))

(defmacro and (&rest exprs)
  (labels ((and-helper (args)
             (if (null (cdr args))
                 (car args)
                 (list 'if (car args) (and-helper (cdr args))))))
    (and-helper exprs)))

(defmacro quasiquote (exp)
  (labels ((qq-list (l)
             (if (consp l)
                 (let ((obj (first l)))
                   (if (and (consp obj) (eq (first obj) 'unquote-splicing))
                       (if (rest l)
                           (list 'append (second obj) (qq-list (rest l)))
                           (second obj))
                       (list 'cons (qq-object obj) (qq-list (rest l)))))
                 (list 'quote l)))
           (qq-element (l)
             (if (eq (first l) 'unquote)
                 (second l)
                 (qq-list l)))
           (qq-object (object)
             (if (consp object)
                 (qq-element object)
                 (list 'quote object))))
    (qq-object exp)))


(defmacro defmacro (name argslist &body body)
  ;; this defmacro adds support for list destructuring in the argslist like:
  ;; (defmacro with-open-file ((var path direction) &body body) ...)
  (let ((expanders))
    (labels ((defmacro-aux (list)
               (when (and list
                          (not (eq '&optional (car list)))
                          (not (eq '&rest (car list)))
                          (not (eq '&body (car list))))
                 (when (consp (car list))
                   (let* ((metavar (gensym))
                          (getter-func metavar))
                     (map1 (lambda (var)
                             (setq expanders (cons `(,var (car ,getter-func)) expanders))
                             (setq getter-func (list 'cdr getter-func)))
                           (car list))
                     (kernel::%rplaca list metavar)))
                 (defmacro-aux (cdr list)))))
      (defmacro-aux argslist))
    `(kernel::%define-macro ,name ,argslist
                            ,@(if expanders
                                  `((let (,@(reverse expanders))
                                      ,@body))
                                  body))))

(defmacro prog1 (&body body)
  (if (null (cdr body))
      (car body)
      (let ((tmp-var-name (gensym)))
        `(let ((,tmp-var-name ,(car body)))
           ,@(cdr body)
           ,tmp-var-name))))

(defmacro or (&rest exprs)
  (labels ((or-helper (args)
             (if (null (cdr args))
                 (car args)
                 (let ((tmp-var-name (gensym)))
                   `(let ((,tmp-var-name ,(car args)))
                      (if ,tmp-var-name
                          ,tmp-var-name
                          ,(or-helper (cdr args))))))))
    (or-helper exprs)))

(defun <= (&rest vals)
  (labels ((aux (a rest)
             (cond ((null rest) t)
                   ((not (> a (car rest))) (aux (car rest) (cdr rest)))
                   (t nil))))
    (aux (car vals) (cdr vals))))

(defun >= (&rest vals)
  (labels ((aux (a rest)
             (cond ((null rest) t)
                   ((not (< a (car rest))) (aux (car rest) (cdr rest)))
                   (t nil))))
    (aux (car vals) (cdr vals))))

(defmacro while (expr &body body)
  (let ((tag-loop (gensym "WHILE-TAG")))
    `(tagbody
        ,tag-loop
        (when ,expr
          ,@body
          (go ,tag-loop)))))

(defmacro until (expr &body body)
  (let ((tag-loop (gensym "UNTIL-TAG")))
    `(tagbody
        ,tag-loop
        (unless ,expr
          ,@body
          (go ,tag-loop)))))

(defun numberp (object)
  ;; we only support fixnums currently!
  (eq 'fixnum (type-of object)))

(defun fixnump (object)
  (eq 'fixnum (type-of object)))

(defun eql (x y) (eq x y))

(defun equal (x y)
  (cond ((eql x y)
         t)
        ((and (stringp x) (stringp y))
         (string= x y))
        ((and (consp x) (consp y))
         (and (equal (car x) (car y))
              (equal (cdr x) (cdr y))))))

(defun %case-generator (test-fn keyform body &optional fail-fn var)
  (let ((tmp-val-name (or var (gensym)))
        (tests))
    (labels ((test-generator (lst)
               (if lst
                   (let ((the-case (car lst)))
                     (setq tests (cons (car the-case) tests))
                     (if (or (eq 't (car the-case))
                             (eq 'otherwise (car the-case)))
                         `(progn ,@(cdr the-case))
                         `(if ,(funcall test-fn tmp-val-name (car the-case))
                              (progn ,@(cdr the-case))
                              ,(test-generator (cdr lst)))))
                   (when fail-fn (funcall fail-fn tmp-val-name (reverse! tests))))))
      `(let ((,tmp-val-name ,keyform))
         ,(test-generator body)))))


(defun %case (keyform errorp body var)
  (%case-generator (lambda (sym-name case-value) `(eql ,sym-name ',case-value))
                   keyform
                   body
                   errorp
                   var))

(defmacro case (keyform &body body)
  (%case keyform nil body nil))

(defmacro caselet ((var keyform) &body body)
  (%case keyform nil body var))

(defmacro ecase (keyform &body body)
  (%case keyform
         (lambda (sym-name case-tests)
           `(signal 'simple-error "ECASE fell through: " ,sym-name ',case-tests))
         body
         nil))

(defun %typecase (keyform errorp body var)
  (%case-generator (lambda (sym-name type-name)
                     (cond ((eq 'list type-name)
                            `(listp ,sym-name))
                           ((eq 'array type-name)
                            `(arrayp ,sym-name))
                           ((eq 'string type-name)
                            `(stringp ,sym-name))
                           (t
                            `(eq ',type-name (type-of ,sym-name)))))
                   keyform
                   body
                   errorp
                   var))

(defmacro typecase (keyform &body body)
  (%typecase keyform nil body nil))

(defmacro typecaselet ((var keyform) &body body)
  (%typecase keyform nil body var))

(defmacro etypecase (keyform &body body)
  (%typecase keyform
             (lambda (sym-name case-tests)
               `(signal 'simple-error "ETYPECASE fell through: " ,sym-name ',case-tests))
             body
             nil))

(let ((setf-functions nil))

  (defun get-setf-functions ()
    (copy-list setf-functions))

  (defun %defsetf (access-fn update-fn)
    (setq setf-functions (cons (cons access-fn update-fn) setf-functions))
    access-fn)

  (defun get-setf-expansion (form)
    (flet ((err (e) (error "no SETF expansion for" e)))
      (cond ((symbolp form)
             `(setq ,form))
            ((consp form)
             (let ((set-functions (assoc (car form) setf-functions)))
               (if set-functions
                   (append (list (cdr set-functions)) (rest form))
                   (err (car form)))))
            (t
             (err form))))))

(defmacro defsetf (access-fn update-fn)
  (%defsetf access-fn update-fn)
  (list 'quote access-fn))

(defun %set-car (cons obj)
  (kernel::%rplaca cons obj)
  obj)
(defsetf car %set-car)

(defun %set-cdr (cons obj)
  (kernel::%rplacd cons obj)
  obj)
(defsetf cdr %set-cdr)

(defmacro setf (place value)
  (append (get-setf-expansion place) (list value)))

(defmacro dolist ((var-name list) &body body)
  (let ((tag-loop (gensym "TAG-LOOP")))
    `(let ((,var-name ,list))
       (tagbody
          ,tag-loop
          (when ,var-name
            (let ((,var-name (car ,var-name)))
              ,@body)
            (setf ,var-name (cdr ,var-name))
            (go ,tag-loop))))))

(defmacro dotimes ((var-name times) &body body)
  (let ((times-name (gensym))
        (tag-loop (gensym "TAG-LOOP")))
    `(let ((,times-name ,times)
           (,var-name 0))
       (tagbody
          ,tag-loop
          (when (< ,var-name ,times-name)
            ,@body
            (incf ,var-name)
            (go ,tag-loop))))))

(defmacro push (obj place)
  `(setf ,place (cons ,obj ,place)))

(defun push! (obj place)
  (let ((original (car place)))
    (setf (car place) obj)
    (setf (cdr place) (cons original (cdr place)))
    place))

(defmacro pop (place)
  `(prog1 (car ,place)
     (setf ,place (cdr ,place))))

(defun pop! (place)
  (let ((val (car place)))
    (setf (car place) (second place))
    (setf (cdr place) (cddr place))
    val))

(defmacro make-array (length &optional (element-type t))
  (list 'kernel::%make-array length element-type))

(defun make-array (length &optional (element-type t))
  (make-array length element-type))

(defmacro array-type (array) (list 'kernel::%array-type array))

(defun array-type (array) (array-type array))

(defmacro aref (array subscript) (list 'kernel::%aref array subscript))

(defun aref (array subscript) (aref array subscript))

(defsetf aref kernel::%aset)

(defmacro array-length (array) (list 'kernel::%array-length array))

(defun array-length (array) (array-length array))

(defun arrayp (obj)
  (let ((type (type-of obj)))
    (when (consp type)
      (cond ((eq 'array (car type))
             t)
            ((eq 'simple-array (car type))
             t)))))

(defun array (&rest vals)
  (let ((array (make-array (length vals)))
        (i 0))
    (while vals
           (setf (aref array i) (pop vals))
           (setf i (+ i 1)))
    array))

(defun nth (n list)
  (if (and list (/= 0 n))
      (nth (- n 1) (cdr list))
      (car list)))

(defun set-nth (n list value)
  (if (and list (/= 0 n))
      (set-nth (- n 1) (cdr list) value)
      (setf (car list) value)))

(defsetf nth set-nth)

(defun listp (obj)
  (if (null obj)
      t
      (consp obj)))

(defun length (sequence)
  (cond ((listp sequence) (list-length sequence))
        ((arrayp sequence) (array-length sequence))))

(defun elt (sequence index)
  (cond ((listp sequence) (nth index sequence))
        ((arrayp sequence) (aref sequence index))))

(defun set-elt (sequence index value)
  (cond ((listp sequence) (setf (nth index sequence) value))
        ((arrayp sequence) (setf (aref sequence index) value))))

(defsetf elt set-elt)

(defun funcall (function &rest args)
  (apply function args))

(defun remove-last! (list)
  "Modifies the input LIST removing the final element returning the new list head and the removeditem."
  (cond ((null list)
         (list nil nil))
        ((null (cdr list))
         (list nil (car list)))
        (t
         (let ((cur list)
               (last-element))
           (while (cddr cur)
                  (setf cur (cdr cur)))
           (setf last-element (second cur))
           (when cur
             (setf (cdr cur) nil))
           (list list last-element)))))

(defun apply (function &rest args)
  (let* ((a (remove-last! args))
         (catted (append (first a) (second a))))
    (kernel::%apply function catted)))

(defun max (a b) (if (> a b) a b))

(defun min (a b) (if (< a b) a b))

(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

(defun make-string (&rest chars)
  (let ((str (make-array (length chars) 'character)))
    (dotimes (i (length chars))
      (setf (aref str i) (nth i chars)))
    str))

(defmacro char-code (char) `(kernel::%char-code ,char))
(defun char-code (char) (char-code char))

(defmacro code-char (code) `(kernel::%code-char ,code))
(defun code-char (code) (code-char code))

(defun characterp (object)
  (eq 'character (type-of object)))

(defun char= (x y)
  (unless (characterp x)
    (signal 'type-error 'character x))
  (unless (characterp y)
    (signal 'type-error 'character y))
  (eql x y))

(defun char/= (x y)
  (not (char= x y)))

(defun stringp (object)
  (and (arrayp object)
       (eq 'character (array-type object))))

(defun string= (x y)
  (unless (stringp x)
    (signal 'type-error 'string x))
  (unless (stringp y)
    (signal 'type-error 'string y))
  (when (= (length x) (length y))
    (labels ((func (n)
               (cond ((= n (length x)) t)
                     ((not (eql (aref x n) (aref y n)))
                      nil)
                     (t (func (+ 1 n))))))
      (func 0))))

(defun string/= (x y)
  (not (string= x y)))

(defun substring (string start &optional (end (length string)))
  (setf start (max 0 (if (< start 0)
                         (+ (length string) start)
                         start)))
  (if (> start (length string))
      (make-array 0 'character)
      (let ((str (make-array (- end start) 'character))
            (i 0))
        (while (< start end)
               (setf (aref str i) (aref string start))
               (incf i)
               (incf start))
        str)))

(defun print (object &optional (stm *standard-output*))
  (if #'print-object
      (print-object object stm)
      (kernel::%file-write stm object))
  object)

(defun print-line (object &optional (stm *standard-output*))
  (print object stm)
  (putchar #\Newline)
  object)

(defmacro macro-print (object)
  (print (eval object))
  nil)

(defun read (&optional (stm *standard-input*) (eof-error-p t) eof-value)
  (kernel::%read stm eof-error-p eof-value))

(defmacro unwind-protect (protected &body cleanup)
  (let ((args (gensym "ARGS"))
        (result (gensym "RESULT"))
        (sig (gensym "SIG"))
        (clean-throw (gensym "CLEAN-THROW")))
    `(let ((,result))
       (handler-case
           (progn (setf ,result ,protected)
                  (signal ',clean-throw))
         (t (,sig &rest ,args)
           ,@cleanup
           (unless (eq ,sig ',clean-throw)
             (apply #'kernel::%signal ,sig ,args))))
       ,result)))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (t (&rest args)
       (list nil args))))

(defmacro with-open-file ((var path direction) &body body)
  `(let ((,var (kernel::%open ,path ,direction)))
     (unwind-protect (progn ,@body)
       (kernel::%close ,var))))

(defun find-last-of (array value)
  (let ((i (- (length array) 1)))
    (while (and (>= i 0)
                (not (eql value (aref array i))))
           (decf i))
    (if (< i 0) nil i)))

(defun parent-directory (path)
  (let ((idx (find-last-of path #\/)))
    (when idx (substring path 0 idx))))

(defun concatenate-arrays (&rest arrays)
  (let* ((lengths (map #'length arrays))
         (total-length (apply #'+ lengths))
         (new-array (make-array total-length (array-type (first arrays))))
         (new-array-idx 0))
    (dolist (array arrays)
      (dotimes (i (length array))
        (setf (aref new-array new-array-idx) (aref array i))
        (incf new-array-idx)))
    new-array))

(defun concatenate (first &rest rest)
  (etypecase first
    (cons (apply #'append first rest))
    (array (apply #'concatenate-arrays first rest))))

(defmacro defconstant (symbol value &optional doc)
  `(setq ,symbol ,value))

(defmacro defvar (symbol &optional value doc)
  `(setq ,symbol ,value))

(defun load (file-path)
  (let* ((here-path *file-path*)
         (here-package *package*)
         (here-dir (get-working-directory))
         (full-path (if (eql #\/ (aref file-path 0))
                        file-path
                        (concatenate here-dir "/" file-path)))
         (there-dir (change-directory (parent-directory full-path))))
    (setq *file-path* full-path)
    (when there-dir
      (unwind-protect
           (with-open-file (file full-path 'read)
             (if (kernel::%file-ok-p file)
                 (until (kernel::%file-eof-p file)
                   (handler-case
                       (kernel::%eval (read file t))
                     (end-of-file () 'ok)))
                 (signal 'load-error "Cannot open file" file-path full-path)))
        (change-directory here-dir)
        (in-package here-package)
        (setq *file-path* here-path)))))

(defun %keylist-member (member list)
  (labels ((aux (list)
             (when list
               (if (eq member (car list))
                   list
                   (aux (cddr list))))))
    (aux list)))

(defmacro lambda (lambda-list &body body)
  (let ((has-&key-p (member '&key lambda-list)))
    (if has-&key-p
        (let ((key-args lambda-list)
              (new-lambda-list)
              (key-arg-names)
              (key-arg-keywords)
              (key-arg-defaults)
              (allow-other-keys-p)
              (rest-variable)
              (verify-key-args-code))
          (until (or (null key-args)
                     (eq '&key (car key-args)))
            (push (car key-args) new-lambda-list)
            (when (eq '&rest (car key-args))
              (setq rest-variable (second key-args)))
            (setq key-args (cdr key-args)))

          (when (eq '&key (car key-args))
            (setq key-args (cdr key-args)))

          (while key-args
            (cond
              ((or (eq '&rest (car key-args)) (eq '&body (car key-args)))
               (signal 'simple-error
                       "Malformed lambda list, unexpected value in keyword-args list"
                       (car key-args)))
              ((eq '&allow-other-keys (car key-args))
               (setq allow-other-keys-p t)
               (when (cdr key-args)
                 (signal 'simple-error "&ALLOW-OTHER-KEYS is only allowed in the final argument position."))
               (setq key-args nil))
              ((symbolp (car key-args))
               (push (car key-args) key-arg-names)
               (push nil key-arg-defaults))
              ((consp (car key-args))
               (push (caar key-args) key-arg-names)
               (push (cadar key-args) key-arg-defaults))
              (t
               (signal 'simple-error "Unexpected expression in lambda list" (car key-args))))
            (setq key-args (cdr key-args)))
          (unless rest-variable
            (setq rest-variable (gensym))
            (push '&rest new-lambda-list)
            (push rest-variable new-lambda-list))
          (setq key-arg-names (reverse! key-arg-names))
          (setq key-arg-keywords (map1 (lambda (s) (intern (symbol-name s) "KEYWORD"))
                                       key-arg-names))
          (setq key-arg-defaults (reverse! key-arg-defaults))
          (setq new-lambda-list (reverse! new-lambda-list))
          (unless allow-other-keys-p
            (setq verify-key-args-code
                  (let ((tmp-var (gensym "TEMP")))
                    `(let ((,tmp-var ,rest-variable))
                         (while ,tmp-var
                          (cond ((member (car ,tmp-var)
                                         '(,@key-arg-keywords :allow-other-keys))
                                 (setq ,tmp-var (cddr ,tmp-var)))
                                ((second (member :allow-other-keys ,rest-variable))
                                 (setq ,tmp-var nil))
                                (t
                                 (signal 'simple-error "Keyword argument is not one of" ',key-arg-keywords
                                        (car ,tmp-var)))))))))
          `(kernel::%lambda ,new-lambda-list
             (let* (,@(map
                        (lambda (symbol keyword default)
                          (setq default
                                (if default
                                    `(or (second (lispyboi::%keylist-member ,keyword ,rest-variable))
                                         ,default)
                                    `(second (lispyboi::%keylist-member ,keyword ,rest-variable))))
                          (list symbol default))
                        key-arg-names
                        key-arg-keywords
                        key-arg-defaults))
               ,verify-key-args-code
               ,@body)))
        (cons 'kernel::%lambda (cons lambda-list body)))))

(load "modules.lisp")
(provide "boot")
(require "stdlib")
