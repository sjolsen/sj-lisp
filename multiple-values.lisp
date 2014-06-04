;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."
;;;
;;;; Macros and functions to flesh out support for multiple values.
;;;
;;;; FIXME: Make declarations work with MULTIPLE-VALUE-LET(*)

(in-package :sj-lisp)


;;;; Function MULTIPLE-VALUE-MAPCAR

;;; Acts just like Common Lisp's MAPCAR, except that it collects multiple values
;;; from the mapped function. For example:
;;;
;;;    (MULTIPLE-VALUE-MAPCAR 2 #'TRUNCATE '(12 23 34) '(10 10 10))
;;;    => (1 2 3), (2 3 4)
;;;
;;; The number of multiple values to return is specified to simplify
;;; implementation and potentially avoid a lot of consing.
(defun multiple-value-mapcar (n func &rest lists)
  "Map FUNC across LISTS, accumulating the first N values returned by each
invocation of FUNC."
  (labels ((unzip-lists (lists)
             (loop
                for list in lists
                when (not list)
                  do (return (list nil nil))
                collecting (car list) into cars
                collecting (cdr list) into cdrs
                finally (return (list cars cdrs)))))
    (loop
       with accumulator = (make-list n)
       for (args rest-lists) = (unzip-lists lists) then (unzip-lists rest-lists)
       while args
       for results = (multiple-value-list (apply func args))
       do (loop
             for acc-cell on accumulator
             for res-cell = results then (cdr res-cell) ;; Don't stop looping at
                                                        ;; the end of results
             do (push (car res-cell) (car acc-cell)))
       finally (return (apply #'values (mapcar #'nreverse accumulator))))))


;;;; Macros MULTIPLE-VALUE-LET*, MULTIPLE-VALUE-LET

;;; Helper for MULTIPLE-VALUE-LET(*).
(defun destructure-mvl-binding (binding)
  "Returns the variables and associated values-form described by BINDING."
  (cond ((listp binding)
         (split-list binding 1 t))
        ((symbolp binding)
         (values (list binding) nil))
        (t (error 'type-error
                  :expected-type '(or list symbol)
                  :datum binding))))

;;; Bind the variables listed sequentially. Each binding consists of a list; the
;;; last element is the form from which the values will be derived, and the rest
;;; are the variables to be bound. For example:
;;;
;;;    (MULTIPLE-VALUE-LET* ((A B (TRUNCATE 42 10))  ;; A = 4, B = 2
;;;                          (C D E (VALUES 1 2 3))) ;; C = 1, D = 2, E = 3
;;;      (+ A B C D E))
;;;    => 12
;;;
;;; Note that in the case where only one variable is bound in a
;;; MULTIPLE-VALUE-LET* form, the syntax is equivalent to plain LET*:
;;;
;;;    (MULTIPLE-VALUE-LET* ((X 3)
;;;                          (Y (+ X 2)))
;;;      (* X Y))
;;;    ==
;;;    (LET* ((X 3)
;;;           (Y (+ X 2)))
;;;      (* X Y))
;;;
;;; Uninitialized bindings are supported using the same convetion as LET*:
;;;
;;;    (MULTIPLE-VALUE-LET* (A (B C (VALUES 1 2)) D)
;;;      (LIST A B C D))
;;;    => (NIL 1 2 NIL)
;;;
;;; See also MULTIPLE-VALUE-LET.
(defmacro multiple-value-let* ((&rest bindings) &body body)
  "Bind the specified variables to the specified values sequentially."
  (if bindings
      (multiple-value-bind (bound-variables values-form)
          (destructure-mvl-binding (first bindings))
        `(multiple-value-bind ,bound-variables ,(car values-form)
           (multiple-value-let* ,(rest bindings)
             ,@body)))
      `(progn ,@body)))

;;; This macro is to MULTIPLE-VALUE-LET* as Common Lisp's LET is to LET*. The
;;; syntax and restrictions described for MULTIPLE-VALUE-LET* apply here as well.
(defmacro multiple-value-let ((&rest bindings) &body body)
  "Bind the specified variables to the specified values in parallel."
  (labels ((destructure-binding-extra (binding)
             ;; Returns a list of temporary variables for the binding; a list of
             ;; the bound variables specified by the binding; and the form to
             ;; evaluate for the binding
             (multiple-value-bind (bound-variables values-form)
                 (destructure-mvl-binding binding)
               (values (mapcar (lambda (symbol)
                                 (gensym (symbol-name symbol)))
                               bound-variables)
                       bound-variables
                       values-form))))
    (multiple-value-bind (temporary-names bound-variables values-forms)
        (multiple-value-mapcar 3 #'destructure-binding-extra bindings)
      `(multiple-value-let* ,(mapcar #'append temporary-names values-forms)
         (let ,(mapcar #'list (apply #'append bound-variables) (apply #'append temporary-names))
           ,@body)))))
