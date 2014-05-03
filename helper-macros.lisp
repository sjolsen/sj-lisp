;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."
;;;
;;;; Macros for general cleaning up of code. These don't really accomplish
;;;; anything particularly complex, but it's nice to have them.

(in-package :sj-lisp)


;;;; Macro FBIND

;;; Locally binds the given names' SYMBOL-VALUEs as a function. This is helpful
;;; when taking a function as a parameter and you really don't feel like using
;;; FUNCALL and APPLY everywhere.
(defmacro fbind ((&rest names) &body body)
  "Bind NAMES' values as local functions"
  `(flet ,(mapcar (lambda (name)
                    (let ((args (gensym)))
                      `(,name (&rest ,args)
                         (apply ,name ,args))))
                  names)
     ,@body))


;;;; Macro WITH-HYGIENIC-NAMES

;;; This is similar to a WITH-GENSYMS–like macro, except that rather than simply
;;; binding each name's value to a gensym, it walks the body, replacing the
;;; names.
(defmacro with-hygienic-names ((&rest names) &body body)
  "Sanitizes the body by replacing each instance of each given NAME with a
unique symbol."
  (labels ((replace-names (form name-alist)
             (cond ((consp form)
                    (cons (replace-names (car form) name-alist)
                          (replace-names (cdr form) name-alist)))
                   ((symbolp form)
                    (let ((new-name (assoc form name-alist)))
                      (or (cdr new-name) form)))
                   (t form))))
    (let ((name-alist
           (loop
              for name in names
              for new-name = (gensym (symbol-name name))
              collecting (cons name new-name))))
      `(progn
         ,@(replace-names body name-alist)))))
