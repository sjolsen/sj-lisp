;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."
;;;
;;;; Macros for general cleaning up of code. These don't really accomplish
;;;; anything particularly complex, but it's nice to have them.
;;;
;;;; FIXME: Consider adding support for BINDing keyword arguments, e.g.:
;;;;
;;;;    (MAPCAR (BIND #'POSITION :2 '(1 2 3 2 1) :FROM-END T) '(1 2 3))
;;;;    => (4 3 2)
;;;
;;;; FIXME?: BIND searches for bindings in a fairly inefficient way (O(n²) in
;;;; the number of supplied arguments), but hey, it's just a macro, and the
;;;; likelihood of a use of the macro being long enough to make any sort of
;;;; difference is small.

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


;;;; Macro BIND

;;; Bind arguments to a function. This can be thought of as a generalization of
;;; currying, but is mostly just to cut down on the proliferation of lambda
;;; forms which simply bind an argument. Arguments are supplied with numeric
;;; keywords, e.g.
;;;
;;;    (FUNCALL (BIND #'NTH :2 '(1 2 3 4)) 0)
;;;    => 1
;;;
;;; This is implemented as a macro rather than a function because processing the
;;; positional arguments is _not_ a trivial operation. The odds of it making a
;;; difference in terms of usability are, I think, rather slim.
(defmacro bind (function &rest keyed-args)
  "Bind FUNCTION with the arguments specified by KEYED-ARGS. The arguments are,
as the name suggests, keyed, specifically by position, starting from 1."
  (labels ((keyword->index (keyword)
             (let ((index (parse-integer (symbol-name keyword))))
               (if (< index 1)
                   (error 'type-error :expected-type '(integer 1) :datum index)
                   index))))
    (multiple-value-bind (bindings greatest-position)
        (loop
           for (position-keyword arg-form) on keyed-args by #'cddr
           for position = (keyword->index position-keyword)
           maximizing position into greatest-position
           collecting (cons position arg-form) into bindings
           finally (return (values bindings greatest-position)))
      (multiple-value-bind (lambda-list arg-forms)
          (loop
             for i from 1 to greatest-position
             for binding = (find i bindings :key #'car)
             for arg-form = (cdr binding)
             for named-argument = (unless binding
                                    (gensym (format nil "ARG~D-" i)))
             if binding
               collect arg-form into arg-forms
             else
               collect named-argument into lambda-list and
               collect named-argument into arg-forms
             finally (return (values lambda-list arg-forms)))
        (with-hygienic-names (rest)
          `(lambda (,@lambda-list &rest rest)
             (apply ,function ,@arg-forms rest)))))))
