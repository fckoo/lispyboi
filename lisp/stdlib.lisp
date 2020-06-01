(defmacro cons (x y) (%cons '%cons (%cons x (%cons y nil))))
(setq cons (lambda (x y) (cons x y)))

(setq list (lambda (&rest lst) lst))

(defmacro defun (name argslist &rest body)
  (list 'setq name (cons 'lambda (cons argslist body))))

(defmacro car (obj) (list '%car obj))
(defun car (obj) (car obj))

(defmacro cdr (obj) (list '%cdr obj))
(defun cdr (obj) (cdr obj))

(defmacro eq (x y) (list '%eq x y))
(defun eq (x y) (eq x y))

(defmacro type-of (obj) (list '%type-of obj))
(defun type-of (obj) (type-of obj))

(defmacro read () (list '%read))
(defun read () (read))

(defmacro macro-expand (expr) (list '%macro-expand expr))
(defun macro-expand (expr) (macro-expand expr))

(defmacro eval (expr) (list '%eval (macro-expand expr)))
(defun eval (expr) (eval expr))

(defmacro apply (func &rest args)
  (cons '%apply (cons func args)))
'(defun apply (func &rest args)
  ;; @FIXME: this is incorrect.
  (eval (cons '%apply (cons (list 'quote func) args))))

(defmacro - (&rest vals) (cons '%- vals))
(defun - (&rest vals) (apply %- vals))

(defmacro + (&rest vals) (cons '%+ vals))
(defun + (&rest vals) (apply %+ vals))

(defmacro * (&rest vals) (cons '%* vals))
(defun * (&rest vals) (apply %* vals))

(defmacro < (&rest vals) (cons '%< vals))
(defun < (&rest vals) (apply %< vals))

(defmacro = (&rest vals) (cons '%= vals))
(defun = (&rest vals) (apply %= vals))

(defmacro > (&rest vals) (cons '%> vals))
(defun > (&rest vals) (apply %> vals))

(defmacro <= (&rest vals) (list 'not (cons '%> vals)))
(defun <= (&rest vals) (not (apply %> vals)))

(defmacro >= (&rest vals) (list 'not (cons '%< vals)))
(defun >= (&rest vals) (not (apply %< vals)))

(defmacro /= (&rest vals) (list 'not (cons '%= vals)))
(defun /= (&rest vals) (not (apply %= vals)))

(defmacro putchar (character) (list '%putchar character))
(defun putchar (character) (putchar character))

(defun null (obj) (eq nil obj))
(defun not (obj) (if obj nil t))
(defun cadr (lst) (car (cdr lst)))
(defun cadar (lst) (car (cdr (car lst))))
(defun caddr (lst) (car (cdr (cdr lst))))
(defun cadddr (lst) (car (cdr (cdr (cdr lst)))))
(defun caddddr (lst) (car (cdr (cdr (cdr (cdr lst))))))
(defun caar (lst) (car (car lst)))
(defun caaar (lst) (car (car (car lst))))
(defun caadr (lst) (car (car (cdr lst))))
(defun caaadr (lst) (car (car (car (cdr lst)))))
(defun caaaadr (lst) (car (car (car (car (cdr lst))))))
(defun cddr (lst) (cdr (cdr lst)))
(defun cdddr (lst) (cdr (cdr (cdr lst))))
(defun cddddr (lst) (cdr (cdr (cdr (cdr lst)))))
(defun cdddddr (lst) (cdr (cdr (cdr (cdr (cdr lst))))))
(defun first (lst) (car lst))
(defun rest (lst) (cdr lst))
(defun second (lst) (cadr lst))
(defun third (lst) (caddr lst))
(defun fourth (lst) (cadddr lst))
(defun fifth (lst) (caddddr lst))

(defmacro gensym (hint) (list '%gensym hint))
(defun gensym (hint) (gensym hint))

(defmacro exit (n) (list '%exit n))
(defun exit (n) (exit n))

(defun append (x y)
  (if x
      (cons (car x) (append (cdr x) y))
      y))

(defun map1 (func seq)
  (if seq (cons (func (car seq)) (map1 func (cdr seq)))))

(defun map (func &rest seqs)
  (if (null (cdr seqs))
      (map1 func (car seqs))
      (if (car seqs)
          (cons (apply func (map1 car seqs))
                (apply map func (map1 cdr seqs))))))

(defmacro let (args &rest body)
  (cons (cons 'lambda (cons (map first args) body))
        (map second args)))

(defmacro flet (definitions &body body)
  (let ((names (map first definitions))
        (lambda-lists (map second definitions))
        (bodies (map cddr definitions)))
    (cons (cons 'lambda (cons names body))
          (map (lambda (ll body) (cons 'lambda (cons ll body)))
               lambda-lists
               bodies))))

(defmacro labels (definitions &body body)
  (let ((names (map first definitions))
        (lambda-lists (map second definitions))
        (bodies (map cddr definitions)))
    (let ((setqs (map (lambda (name ll body)
                        (list 'setq name (cons 'lambda (cons ll body))))
                      names
                      lambda-lists
                      bodies)))
      (cons (cons 'lambda (cons names (append setqs body)))
            nil))))

(defmacro and (&rest exprs)
  (labels ((and-helper (args)
             (if (null (cdr args))
                 (car args)
                 (list 'if (car args) (and-helper (cdr args))))))
    (and-helper exprs)))

(defun consp (obj) (eq 'cons (type-of obj)))
(defun symbolp (obj) (eq 'symbol (type-of obj)))

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

(defmacro progn (&body body)
  (if (null (cdr body))
      (car body)
      `(let () ,@body)))

(defmacro prog1 (&body body)
  (if (null (cdr body))
      (car body)
      (let ((tmp-var-name (gensym)))
        `(let ((,tmp-var-name ,(car body)))
           ,@(cdr body)
           ,tmp-var-name))))

(defmacro when (test &body body)
  `(if ,test (progn ,@body) nil))

(defmacro unless (test &body body)
  `(if ,test nil (progn ,@body)))

(defmacro cond (&body body)
  (if (null body)
      nil
      `(if ,(caar body)
           (progn ,@(rest (first body)))
           (cond ,@(rest body)))))

(defun assoc (item alist)
  (when alist
    (if (eq item (caar alist))
        (car alist)
        (assoc item (cdr alist)))))

(defun numberp (object)
  ;; we only support fixnums currently!
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

(defun %case-generator (test-fn keyform body)
  (let ((tmp-val-name (gensym)))
    (labels ((test-generator (lst)
               (when lst
                 (let ((the-case (car lst)))
                   (if (eq 't (car the-case))
                       `(progn ,@(cdr the-case))
                       (if (eq 'otherwise (car the-case))
                           `(progn ,@(cdr the-case))
                           `(if ,(test-fn tmp-val-name (car the-case))
                                (progn ,@(cdr the-case))
                                ,(test-generator (cdr lst)))))))))
      `(let ((,tmp-val-name ,keyform))
         ,(test-generator body)))))

(defmacro case (keyform &body body)
  (%case-generator (lambda (sym-name case-value) `(eql ,sym-name ',case-value))
                   keyform
                   body))

(defmacro typecase (keyform &body body)
  (%case-generator (lambda (sym-name type-name)
                     (print type-name)
                     (cond ((eq 'list type-name)
                            `(listp ,sym-name))
                           (t
                            `(eq ',type-name (type-of ,sym-name)))))
                   keyform
                   body))

(let ((*setf-functions* nil))
  (defun %defsetf (access-fn update-fn)
    (setq *setf-functions* (cons (cons access-fn update-fn) *setf-functions*))
    access-fn)

  (defun get-setf-expansion (form)
    (cond ((symbolp form)
           form)
          ((consp form)
           (let ((set-functions (assoc (car form) *setf-functions*)))
             (when set-functions
               (append (list (cdr set-functions))
                       (rest form)))))
          (t
           ;; @TODO: Need to implement error capabilities in host
           nil))))

(defmacro defsetf (access-fn update-fn)
  (%defsetf access-fn update-fn)
  `(quote ,access-fn))

(defsetf car %set-car)
(defsetf cdr %set-cdr)

(defmacro setf (place value)
  (let ((expansion (get-setf-expansion place)))
    (cond ((symbolp expansion)
           `(setq ,expansion ,value))
          ((consp expansion)
           (append expansion (list value)))
          (t
           ;; @TODO: Need to implement error capabilities in host
           nil))))


(defmacro dolist (var-list &body body)
  (let ((var-name (first var-list))
        (list (second var-list))
        (fn-name (gensym))
        (list-name (gensym)))
    `(labels ((,fn-name (,list-name)
                (when ,list-name
                  (let ((,var-name (car ,list-name)))
                    ,@body
                    (,fn-name (cdr ,list-name))))))
       (,fn-name ,list))))

(defmacro dotimes (var-times &body body)
  (let ((var-name (first var-times))
        (times-name (gensym))
        (times (second var-times))
        (fn-name (gensym)))
    `(labels ((,fn-name (,var-name ,times-name)
                (when (< ,var-name ,times-name)
                  ,@body
                  (,fn-name (+ ,var-name 1) ,times-name))))
       (,fn-name 0 ,times))))

(defmacro push (obj place)
  `(setf ,place (cons ,obj ,place)))
(defun push (obj place)
  (let ((original (car place)))
    (setf (car place) obj)
    (setf (cdr place) (cons original (cdr place)))
    place))

(defmacro pop (place)
  `(prog1 (car ,place)
     (setf ,place (cdr ,place))))

(defun pop (place)
  (let ((val (car place)))
    (setf (car place) (second place))
    (setf (cdr place) (cddr place))
    val))

(defmacro make-array (length default-value)
  (if default-value
      (list '%make-array length default-value)
      (list '%make-array length)))
(defun make-array (length default-value)
  (make-array length default-value))

(defmacro array-type (array) (list '%array-type array))
(defun array-type (array) (array-type array))

(defmacro aref (array subscript) (list '%aref array subscript))
(defun aref (array subscript) (aref array subscript))
(defsetf aref %set-aref)
(defmacro array-length (array) (list '%array-length array))
(defun array-length (array) (array-length array))
(defun arrayp (obj)
  (let ((type (type-of obj)))
    (when (consp type)
      (cond ((eq 'array (car type))
             t)
            ((eq 'simple-array (car type))
             t)))))

(defun nth (n list)
  (if (and list (/= 0 n))
      (nth (- n 1) (cdr list))
      (car list)))
(defun set-nth (n list value)
  (if (and list (/= 0 n))
      (set-nth (- n 1) (cdr list) value)
      (setf (car list) value)))
(defsetf nth set-nth)
(defun list-length (list)
  (labels ((f (lst accum)
             (if lst
                 (f (cdr lst) (+ 1 accum))
                 accum)))
    (f list 0)))
(defun listp (obj)
  (if (null obj)
      t
      (consp obj)))

(defun length (sequence)
  (cond ((listp sequence) (list-length sequence))
        ((arrayp sequence) (array-length sequence))))

(defun elt (sequence index)
  (cond ((listp sequence)
         (nth index sequence))
        ((arrayp sequence)
         (aref sequence index))))

(defun set-elt (sequence index value)
  (cond ((listp sequence) (setf (nth index sequence) value))
        ((arrayp sequence) (setf (aref sequence index) value))))

(defsetf elt set-elt)

(defun make-string (&rest chars)
  (let ((str (make-array (length chars) 'character)))
    (dotimes (i (length chars))
      (setf (aref str i) (nth i chars)))
    str))

(defun stringp (object)
  (eq 'character (array-type object)))

(defun string= (x y)
  (when (= (length x) (length y))
    (labels ((func (n)
               (cond ((= n (length x)) t)
                     ((not (eql (aref x n) (aref y n)))
                      (print `(different at index ,n with ,(aref x n) and ,(aref y n)))
                      (print (%bits-of (aref x n)))
                      (print (%bits-of (aref y n)))
                      nil)
                     (t (func (+ 1 n))))))
      (func 0))))

(defun string/= (x y)
  (not (string= x y)))

;;(defmacro print (obj) (list '%print obj))
(defun print (object)
  (cond ((stringp object)
         (dotimes (i (array-length object))
           (putchar (aref object i)))
         (putchar #\newline))
        (t (%print object))))

