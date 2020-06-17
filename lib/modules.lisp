;;;
;;; A module is made up of one or more files, a module's loader is a file ending in
;;; '.lisp' or '.module'. It may contain arbitrary code and is expected to call
;;; PROVIDE with its module name at some point.
;;;
;;; Module names are case sensitive
;;;
;;; Inside a module can be regular code, a series of LOADs, a series of REQUIREs
;;; or any combination. For all intents and purposes a module is just plain code
;;; responsible for LOADing its own files in the correct order.
;;;
;;; REQUIREd modules will be LOADed only _once_.
;;;
;;; The global variable *MODULES* contains a list, in reverse load order, of all
;;; loaded modules.
;;;
;;; The global variable *MODULE-LOAD-DIRECTORIES* contains a list of directories
;;; to search for a REQUIREd module. This list is traversed front to back.
;;;
;;; When a module is REQUIREd:
;;;   ? if it has already been loaded -> MODULE-NAME is returned
;;;   ? if it has not already been loaded
;;;     ? if a path to the module was given it will be LOADed
;;;       + the load was successful -> MODULE-NAME is returned
;;;       - the load was unsuccessful -> NIL is returned
;;;     ? if no path to the module was given
;;;       + the *MODULE-LOAD-DIRECTORIES* list will be traveresed looking for the module
;;;         ? if the module is found it will be loaded
;;;           + the load was successful -> MODULE-NAME is returned
;;;           - the load was unsuccessful -> NIL is returned
;;;

(let ((exec-dir (parent-directory (get-executable-path))))
  (defun get-executable-directory ()
    exec-dir))

(setf *module-load-directories*
      (list (concatenate (get-executable-directory) "/lib")
            (concatenate (get-executable-directory) "/lisp")
            (get-executable-directory)))

(setf *modules* ())

(defun provide (module-name)
  (push module-name *modules*))

(defun file-accessible-p (file-path)
  (with-open-file (file file-path 'read)
    (%file-ok-p file)))

(defun find-if (predicate list)
  (when list
    (let ((val (funcall predicate (car list))))
      (if val
          val
          (find-if predicate (cdr list))))))

(defun find-module (module-name)
  (let ((paths *module-load-directories*))
    (find-if (lambda (suffix)
               (find-if (lambda (path)
                          (setf path (concatenate path "/" module-name suffix))
                          (when (file-accessible-p path)
                            path))
                        paths))
             '("" ".lisp" ".module"))))

(defun load-module (module-name path)
  (if path
      (and (load path) module-name)
      (let ((found-module (find-module module-name)))
        (unless found-module
          (error "module not found!" module-name))
        (and (load found-module) module-name))))

(defun require (module-name &optional path)
  (if (member module-name *modules* #'equal)
      module-name
      (load-module module-name path)))

(provide "modules")


