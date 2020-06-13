(defmacro cons (x y) (%cons '%cons (%cons x (%cons y nil))))
(%define-function 'cons (lambda (x y) (cons x y)))

(%define-function 'list (lambda (&rest lst) lst))

(defmacro defun (name argslist &rest body)
  (list '%define-function (list 'quote name) (cons 'lambda (cons argslist body))))

(defmacro car (obj) (list '%car obj))
(defun car (obj) (car obj))

(defmacro cdr (obj) (list '%cdr obj))
(defun cdr (obj) (cdr obj))

(defmacro eq (x y) (list '%eq x y))
(defun eq (x y) (eq x y))

(defmacro type-of (obj) (list '%type-of obj))
(defun type-of (obj) (type-of obj))

(defun consp (obj) (eq 'cons (type-of obj)))

(defun symbolp (obj) (eq 'symbol (type-of obj)))

(defmacro read (stm) (if stm
                         (list '%read stm)
                         (list '%read)))
(defun read (stm) (read stm))

(defmacro macro-expand (expr) (list '%macro-expand expr))
(defun macro-expand (expr) (macro-expand expr))

(defun eval (expr)
  (%eval (macro-expand expr)))

(defmacro apply (func &rest args)
  (cons '%apply (cons func args)))

(defmacro - (&rest vals) (cons '%- vals))
(defun - (&rest vals) (apply #'%- vals))

(defmacro + (&rest vals) (cons '%+ vals))
(defun + (&rest vals) (apply #'%+ vals))

(defmacro * (&rest vals) (cons '%* vals))
(defun * (&rest vals) (apply #'%* vals))

(defmacro / (x y) (list '%/ x y))
(defun / (x y) (/ x y))

(defmacro < (&rest vals) (cons '%< vals))
(defun < (&rest vals) (apply #'%< vals))

(defmacro = (&rest vals) (cons '%= vals))
(defun = (&rest vals) (apply #'%= vals))

(defmacro > (&rest vals) (cons '%> vals))
(defun > (&rest vals) (apply #'%> vals))

(defmacro <= (&rest vals) (list 'not (cons '%> vals)))
(defun <= (&rest vals) (not (apply #'%> vals)))

(defmacro >= (&rest vals) (list 'not (cons '%< vals)))
(defun >= (&rest vals) (not (apply #'%< vals)))

(defmacro /= (&rest vals) (list 'not (cons '%= vals)))
(defun /= (&rest vals) (not (apply #'%= vals)))

(defmacro putchar (character &optional (stm *STANDARD-OUTPUT*))
  (list '%putchar character stm))
(defun putchar (character &optional (stm *STANDARD-OUTPUT*))
  (putchar character stm))

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

(defmacro gensym (&optional (hint "G")) (list '%gensym hint))
(defun gensym (&optional (hint "G")) (gensym hint))

(defmacro symbol-name (symbol) (list '%symbol-name symbol))
(defun symbol-name (symbol) (symbol-name symbol))

(defmacro make-symbol (symbol-name) (list '%make-symbol symbol-name))
(defun make-symbol (symbol-name) (make-symbol symbol-name))

(defmacro intern (symbol-name) (list '%intern symbol-name))
(defun intern (symbol-name) (intern symbol-name))

(defmacro exit (&optional (n 0)) (list '%exit n))
(defun exit (&optional (n 0)) (exit n))

(defmacro signal (tag &rest arguments)
  (cons '%signal (cons tag arguments)))

(defun signal (tag &rest arguments)
  (apply #'%signal tag arguments))

(defun error (message &rest arguments)
  (apply #'%signal 'error message arguments))

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

(defun reverse (list)
  (foldl #'cons nil list))

(defun map1 (func seq)
  ;; NOT TAIL RECURSIVE! later definitions are
  (if seq (cons (funcall func (car seq))
                (map1 func (cdr seq)))))

(defun map (func &rest seqs)
  ;; NOT TAIL RECURSIVE! later definitions are
  (if (null (cdr seqs))
      (map1 func (car seqs))
      (if (car seqs)
          (cons (apply func (map1 #'car seqs))
                (apply #'map func (map1 #'cdr seqs))))))


(defun %print-line (obj)
  (%print obj)
  (%putchar #\newline)
  obj)

(defmacro let (args &body body)
  (cons (cons 'lambda (cons (map #'first args) body))
        (map #'second args)))

(defmacro let* (args &body body)
  (let ((names (map #'first args))
        (vals (map #'second args)))
    (let ((setqs (map (lambda (name val) (list 'setq name val))
                      names
                      vals)))
      (cons (cons 'lambda (cons names (append setqs body)))
            (map (lambda (&rest p) nil) names)))))

(defmacro progn (&body body)
  (if (null (cdr body))
      (car body)
      (cons 'let (cons 'nil body))))


(defmacro when (test &body body)
  (list 'if test (cons 'progn body) nil))

(defmacro unless (test &body body)
  (list 'if test nil (cons 'progn body)))

(defun assoc (item alist)
  (when alist
    (if (eq item (caar alist))
        (car alist)
        (assoc item (cdr alist)))))

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



(defun %flet-transform (old-new-names expr)
  ;; %FLET-TRANSFORM visits every leaf of EXPR and replaces any of the old/user symbols
  ;; in the call position and when referenced by the (FUNCTION SYM) form with its new
  ;; symbol
  (if (consp expr)
      (map (lambda (e)
             (if (consp e)
                 (cond ((eq 'function (car e))
                        (let ((found (assoc (second e) old-new-names)))
                          (if found
                              (list 'function (cdr found))
                              e)))
                       ((eq 'quote (car e))
                        e)
                       (t
                        (let ((found (assoc (car e) old-new-names)))
                          (if found
                              (cons 'funcall (cons (cdr found) (cdr e)))
                              (%flet-transform old-new-names e)))))
                 e))
           expr)
      expr))

;; The basic premise for FLET and LABELS is to create new NOT INTERNED symbols for each
;; function and then walk the form's body and replace instances where the user-defined
;; name is in the call position with the appropriate uninterned symbol. LABELS does
;; exactly the same as FLET but it also transforms the body of the function definitions
;; aswell.
(defmacro flet (definitions &body body)
  (let* ((names (map #'first definitions))
         (new-names (map (lambda (e) (gensym (symbol-name e))) names))
         (old-new-names (map #'cons names new-names))
         (lambda-lists (map #'second definitions))
         (bodies (map #'cddr definitions)))
    (%print-line (map #'first definitions))
    (cons 'let (cons (map (lambda (sym ll body)
                            (list sym (cons 'lambda (cons ll body))))
                          new-names
                          lambda-lists
                          bodies)
                     (%flet-transform old-new-names body)))))



(defmacro labels (definitions &body body)
  (let* ((names (map #'first definitions))
         (new-names (map (lambda (e) (gensym (symbol-name e))) names))
         (old-new-names (map #'cons names new-names))
         (lambda-lists (map #'second definitions))
         (bodies (map #'cddr definitions)))
    (cons 'let* (cons (map (lambda (sym ll body)
                             (list sym (cons 'lambda (cons ll (%flet-transform old-new-names body)))))
                           new-names
                           lambda-lists
                           bodies)
                      (%flet-transform old-new-names body)))))

;; Yes we are redefining MAP1 and MAP because the earlier definitions are not tail recursive
;; and we have some friendlier constructs to define them now
(defun map1 (func seq)
  (labels ((map1-aux (accum list)
             (if list
                 (map1-aux (cons (funcall func (car list)) accum)
                           (cdr list))
                 (reverse accum))))
    (map1-aux nil seq)))

(defun map (func &rest seqs)
  (if (null (cdr seqs))
      (map1 func (car seqs))
      (if (car seqs)
          (labels ((map-aux (accum lists)
                     (if (car lists)
                         (map-aux (cons (apply func (map1 #'car lists)) accum)
                                  (map1 #'cdr lists))
                         (reverse accum))))
            (map-aux nil seqs)))))

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

(defmacro while (expr &body body)
  (let ((fn-name (gensym "WHILE-LOOP")))
    `(labels ((,fn-name ()
                (when ,expr
                  ,@body
                  (,fn-name))))
       (,fn-name))))

(defmacro until (expr &body body)
  (let ((fn-name (gensym "UNTIL-LOOP")))
    `(labels ((,fn-name ()
                (unless ,expr
                  ,@body
                  (,fn-name))))
       (,fn-name))))

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

(defun %case-generator (test-fn keyform body)
  (let ((tmp-val-name (gensym)))
    (labels ((test-generator (lst)
               (when lst
                 (let ((the-case (car lst)))
                   (if (or (eq 't (car the-case))
                           (eq 'otherwise (car the-case)))
                       `(progn ,@(cdr the-case))
                       `(if ,(funcall test-fn tmp-val-name (car the-case))
                            (progn ,@(cdr the-case))
                            ,(test-generator (cdr lst))))))))
      `(let ((,tmp-val-name ,keyform))
         ,(test-generator body)))))

(defmacro case (keyform &body body)
  (%case-generator (lambda (sym-name case-value) `(eql ,sym-name ',case-value))
                   keyform
                   body))

(defmacro typecase (keyform &body body)
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
           (error "no SETF expansion for" form)
           nil))))

(defmacro defsetf (access-fn update-fn)
  (%defsetf access-fn update-fn)
  (list 'quote access-fn))

(defsetf car %set-car)

(defsetf cdr %set-cdr)

(defmacro setf (place value)
  (let ((expansion (get-setf-expansion place)))
    (cond ((symbolp expansion)
           `(setq ,expansion ,value))
          ((consp expansion)
           (append expansion (list value))))))

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
    `(let ((,times-name ,times))
       (labels ((,fn-name (,var-name)
                  (when (< ,var-name ,times-name)
                    ,@body
                    (,fn-name (+ ,var-name 1)))))
         (,fn-name 0)))))

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

(defmacro make-array (length &optional (element-type t))
  (list '%make-array length element-type))

(defun make-array (length &optional (element-type t))
  (make-array length element-type))

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
  (cond ((listp sequence) (nth index sequence))
        ((arrayp sequence) (aref sequence index))))

(defun set-elt (sequence index value)
  (cond ((listp sequence) (setf (nth index sequence) value))
        ((arrayp sequence) (setf (aref sequence index) value))))

(defsetf elt set-elt)

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

(defmacro char-code (char) `(%char-code ,char))
(defun char-code (char) (char-code char))

(defmacro code-char (code) `(%code-char ,code))
(defun code-char (code) (code-char code))

(defun stringp (object)
  (eq 'character (array-type object)))

(defun characterp (object)
  (eq 'character (type-of object)))

(defun string= (x y)
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

(defun print (object &optional (stm *STANDARD-OUTPUT*))
  (cond ((stringp object)
         (dotimes (i (array-length object))
           (putchar (aref object i) stm)))
        (t (%print object stm)))
  object)

(defun print-line (object &optional (stm *STANDARD-OUTPUT*))
  (prog1 (print object stm)
    (putchar #\Newline)))



(defmacro with-open-file (var-path-direction &body body)
  (let ((var (first var-path-direction))
        (path (second var-path-direction))
        (direction (third var-path-direction)))
    `(let ((,var (%open ,path ,direction)))
       (prog1 (progn ,@body)
         (%close ,var)))))

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
  (typecase first
    (cons (apply #'append first rest))
    (array (apply #'concatenate-arrays first rest))))

(defun compile (name definition)
  (if name
      (%define-function name definition)
      definition))

(defun load (file-path)
  (let* ((here-path *file-path*)
         (here-dir (get-working-directory))
         (full-path (if (eql #\/ (aref file-path 0))
                        file-path
                        (concatenate here-dir "/" file-path)))
         (there-dir (change-directory (parent-directory full-path))))
    (setq *file-path* full-path)
    (when there-dir
      ;;(unwind-protect)
      (with-open-file (file full-path 'read)
        (if (%file-ok-p file)
            (progn
              (until (%file-eof-p file)
                     (eval (read file)))
              full-path)
            (signal 'load-error "Cannot open file" file-path)))
      (change-directory here-dir)
      (setq *file-path* here-path))))

(load "modules.lisp")
(provide "boot")


