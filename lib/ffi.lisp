(provide "ffi")

(setq *ffi-pointer-size* 8)

(setf *ffi-bytespecs* '(uint8 byte
                        uint16
                        uint32
                        uint64
                        char character string
                        short
                        int
                        long))

(setq *ffi-type-registry*
      (list '(uint8 1 nil)
            '(uint16 2 nil)
            '(uint32 4 nil)
            '(uint64 8 nil)
            '(char 1 nil)
            '(short 2 nil)
            '(int 4 nil)
            '(long 8 nil))
      "A list of (TYPE-NAME TYPE-SIZE DEFINITION)")

(defun ffi-get-type (type)
  (assoc type *ffi-type-registry*))

(defun %ffi-pointer-type-p (type)
  (let ((sym-name (symbol-name type)))
    (eql #\* (aref sym-name (- (length sym-name) 1)))))

(defun %ffi-pointer-align-up (offset)
  (+ (rem offset *ffi-pointer-size*) offset))

(defun %ffi-sizeof (type)
  (if (%ffi-pointer-type-p type)
      *ffi-pointer-size*
      (second (assoc type *ffi-type-registry*))))

(defmacro ffi-sizeof (type)
  (%ffi-sizeof type))

(defun ffi-field-offsets (type-obj)
  (third type-obj))

(defmacro ffi-offset-of (type field)
  (let ((type-obj (ffi-field-offsets
                   (if (symbolp type)
                       (ffi-get-type type)
                       type))))
    (third (assoc field type-obj))))

(defmacro ffi-type-of (type field)
  (let ((type-obj (ffi-field-offsets
                   (if (symbolp type)
                       (ffi-get-type type)
                       type))))
    (list 'quote (second (assoc field type-obj)))))

(defun %ffi-getter-function (type-size)
  (case type-size
    (1 'ffi-ref-8)
    (2 'ffi-ref-16)
    (4 'ffi-ref-32)
    (8 'ffi-ref-64)
    (t 'ffi-ref)))

(defun %ffi-setter-function (type-size)
  (case type-size
    (1 'ffi-set-ref-8)
    (2 'ffi-set-ref-16)
    (4 'ffi-set-ref-32)
    (8 'ffi-set-ref-64)
    (t 'ffi-set-ref)))

(defmacro ffi-defstruct (struct-name &body body)
  (let* ((field-names (map1 #'first body))
         (field-sizes (map1 (lambda (e) (%ffi-sizeof (second e))) body))
         (field-types (map1 #'second body))
         (total-size 0)
         (field-offsets (let ((offset 0))
                          (map (lambda (field field-type size)
                                 (incf total-size size)
                                 (when (%ffi-pointer-type-p field-type)
                                   (let ((new-offs (%ffi-pointer-align-up offset)))
                                     (incf total-size (- new-offs offset))
                                     (setf offset new-offs)))
                                 (prog1 (list field field-type offset)
                                   (incf offset size)))
                               field-names
                               field-types
                               field-sizes))))
    (let ((type-obj (list struct-name total-size field-offsets))
          (functions))
      (push type-obj *ffi-type-registry*)
      (map (lambda (name size type offset)
             (let ((offset (third offset))
                   (getter-name (intern (concatenate (symbol-name struct-name) "-GET-" (symbol-name name))))
                   (setter-name (intern (concatenate (symbol-name struct-name) "-SET-" (symbol-name name)))))
               (push 
                `(defun ,getter-name (,struct-name)
                   (,(%ffi-getter-function size) (ffi-ref ,struct-name ,offset)))
                functions)
               (push
                `(defun ,setter-name (,struct-name value)
                   (,(%ffi-setter-function size) (ffi-ref ,struct-name ,offset) value))
                functions)))
           field-names
           field-sizes
           field-types
           field-offsets)
      (push `(defun ,(intern (concatenate "MAKE-" (symbol-name struct-name))) ()
               (ffi-zero-alloc ,total-size))
            functions)
      (push 'progn functions)
      functions)))


(defun ffi-get-symbol-or-signal (library-handle symbol-name)
  (let ((sym (ffi-get-symbol library-handle symbol-name)))
    (unless sym
      (signal 'ffi-symbol-not-found library-handle symbol-name))
    sym))

(defun ffi-nullptr-p (obj)
  (eq (ffi-nullptr) obj))

(defun with-ffi-array ((array-var bytespec buffer-expr) &body body)
  (let ((buffer (gensym))
        (size (gensym))
        (ref-function)
        (ref-offset-calc)
        (tmp-expr (gensym)))
    (if (member bytespec '(char character string))
        (progn
          `(let ((,tmp-expr ,buffer-expr))
             (when ,tmp-expr
               (destructuring-bind (,buffer ,size) ,tmp-expr
                 (unwind-protect (let ((,array-var (prog1 (ffi-coerce-string ,buffer ,size)
                                                     (ffi-free ,buffer))))
                                   ,@body))))))
        (progn
          (case bytespec
            (uint8
             (setf ref-function 'ffi-ref-8)
             (setf ref-offset-calc 'i))
            (uint16
             (setf ref-function 'ffi-ref-16)
             (setf ref-offset-calc '(* i 2)))
            (uint32
             (setf ref-function 'ffi-ref-32)
             (setf ref-offset-calc '(* i 4)))
            (uint64
             (setf ref-function 'ffi-ref-64)
             (setf ref-offset-calc '(* i 8)))
            (byte
             (setf ref-function 'ffi-ref-8)
             (setf ref-offset-calc 'i))
            (short
             (setf ref-function 'ffi-ref-16)
             (setf ref-offset-calc '(* i 2)))
            (int
             (setf ref-function 'ffi-ref-32)
             (setf ref-offset-calc '(* i 4)))
            (long
             (setf ref-function 'ffi-ref-64)
             (setf ref-offset-calc '(* i 8)))
            (t (signal 'bytespec-error "BYTESPEC must be one of" *ffi-bytespecs*)))
          `(let ((,tmp-expr ,buffer-expr))
             (when ,tmp-expr
               (destructuring-bind (,buffer ,size) ,tmp-expr
                 (let ((,array-var (make-array ,size)))
                   (dotimes (i ,size)
                     (setf (aref ,array-var i) (,ref-function (ffi-ref ,buffer ,ref-offset-calc))))
                   (ffi-free ,buffer)
                   ,@body))))))))

(defmacro ffi-with-symbols (library-name symbols &body body)
  (let ((lib-var (gensym)))
    `(let* ((,lib-var (ffi-open ,library-name))
            ,@(map1 (lambda (e)
                      `(,(first e) (ffi-get-symbol-or-signal ,lib-var ,(second e))))
                    symbols))
       (flet (,@(map1 (lambda (e)
                        `(,(first e) (&rest args) (apply #'ffi-call ,(first e) args)))
                  symbols))
         ,@body))))


(defmethod print-object ((o system-pointer) stream)
  (format stream "#<SYSTEM-POINTER ~X>" (ffi-coerce-fixnum o))
  o)
