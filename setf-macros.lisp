;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."
;;;
;;;; Operations for juggling SETF operations, with a focus on avoiding the
;;;; multiple evaluation of places, as much for performance as for correctness.

(in-package :sj-lisp)


;;;; Macro EXCHANGEF

;;; Essentially the same as single-place SETF, but return the _old_ value rather
;;; than the new one. Especially useful for breaking apart lists.
(defmacro exchangef (place value &environment env)
  "Assign value to place, returning place's previous value"
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((old-values (loop
                         repeat (length store-vars)
                         collecting (gensym))))
      `(let ,(mapcar #'list vars vals)
         (multiple-value-bind ,old-values ,reader-form
           (multiple-value-bind ,store-vars ,value
             ,writer-form
             (values ,@old-values)))))))


;;;; Macro SWAPF

;;; Exactly what it says on the tin. The purpose is served by ROTATEF with two
;;; arguments, but it can produce some hairy code. For example, with aggressive
;;; optimization, the following prints out 70 bytes of x86_64 on my machine with
;;; this SWAPF, 252 with the naïve ROTATEF-based implementation:
;;;
;;;    (DISASSEMBLE
;;;      (LAMBDA ()
;;;        (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
;;;        (LET ((X 1) (Y 2)
;;;              (A 3) (B 4))
;;;          (SWAPF (VALUES X Y) (VALUES A B)))))
;;;
;;; The SECOND-VALUE option exists because swapping the order of the arguments
;;; can affect the assigned values. Better that it be done here than that the
;;; whole macro be reimplemented with a one-character difference.
(defmacro swapf (place1 place2 &key second-value &environment env)
  "Swap the values of the two places, returning the first place's new value"
  (multiple-value-bind (vars1 vals1 store-vars1 writer-form1 reader-form1)
      (get-setf-expansion place1 env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
        (get-setf-expansion place2 env)
      (let ((tmp-values (loop
                           repeat (length store-vars1)
                           collecting (gensym))))
        `(let (,@(mapcar #'list vars1 vals1)
               ,@(mapcar #'list vars2 vals2))
           (multiple-value-bind ,tmp-values ,reader-form1
             (multiple-value-bind ,store-vars1 ,reader-form2
               (multiple-value-bind ,store-vars2 (values ,@tmp-values)
                 ,writer-form1
                 ,writer-form2
                 ,(if second-value
                      `(values ,@store-vars2)
                      `(values ,@store-vars1))))))))))
